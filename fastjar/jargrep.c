/*
  jargrep.c - main functions for jargrep utility
  Copyright (C) 2002 Free Software Foundation
  Copyright (C) 1999, 2000 Bryan Burns
  Copyright (C) 2000 Cory Hollingsworth 
 
  Parts of this program are base on Bryan Burns work with fastjar 
  Copyright (C) 1999. 

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

/* Id: jargrep.c,v 1.5 2002/01/03 04:57:56 rodrigc Exp

Log: jargrep.c,v 
Revision 1.5  2002/01/03 04:57:56  rodrigc
2001-01-02  Craig Rodrigues  <rodrigc@gcc.gnu.org>

        PR bootstrap/5117
        * configure.in (AC_CHECK_HEADERS): Check for stdlib.h.
        * Makefile.am: Move grepjar to bin_PROGRAMS.
        * config.h.in: Regenerated.
        * Makefile.in: Regenerated.
        * aclocal.m4: Regenerated.
        * jargrep.c: Eliminate some signed/unsigned and default
        uninitialized warnings. Use HAVE_STDLIB_H instead of
        STDC_HEADERS macro.
        * jartool.c: Likewise.
        * compress.c: Likewise.

Revision 1.4  2000/12/15 18:45:09  tromey
	* jargrep.c: Include getopt.h if it exists.
	(optind): Declare.
	* configure, config.h: Rebuilt.
	* configure.in: Check for getopt.h.

Revision 1.3  2000/12/14 18:45:35  ghazi
Warning fixes:

	* compress.c: Include stdlib.h and compress.h.
	(rcsid): Delete.
	(report_str_error): Make static.
	(ez_inflate_str): Delete unused variable.  Add parens in if-stmt.
	(hrd_inflate_str): Likewise.

	* compress.h (init_compression, end_compression, init_inflation,
	end_inflation): Prototype void arguments.

	* dostime.c (rcsid): Delete.

	* jargrep.c: Include ctype.h, stdlib.h, zlib.h and compress.h.
	Make functions static.  Cast ctype function argument to `unsigned
	char'.  Add parens in if-stmts.  Constify.
	(Usage): Change into a macro.
	(jargrep): Remove unused parameter.

	* jartool.c: Constify.  Add parens in if-stmts.  Align
	signed/unsigned char pointers in functions calls using casts.
	(rcsid): Delete.
	(list_jar): Fix printf format specifier.
	(usage): Chop long string into bits.  Reformat.

	* pushback.c (rcsid): Delete.

Revision 1.2  2000/12/11 02:59:55  apbianco
2000-12-10  Robert Lipe <robertlipe@usa.net>

	* jargrep.c (jargrep): Added null statement after case.

2000-12-10  Alexandre Petit-Bianco  <apbianco@cygnus.com>

	* Makefile: Removed.
	* Makefile.in: Rebuilt with `-i' and `--enable-foreign'.

(http://gcc.gnu.org/ml/gcc/2000-12/msg00294.html)

Revision 1.1  2000/12/09 03:08:23  apbianco
2000-12-08  Alexandre Petit-Bianco  <apbianco@cygnus.com>

        * fastjar: Imported.

Revision 1.8  2000/09/13 14:02:02  cory
Reformatted some of the code to more closly match the layout of the orriginal
fastjar utility.

Revision 1.7  2000/09/12 22:29:36  cory
Jargrep now seems to do what I want it to do.  Performs properly on Linux x86,
will test some other platforms later.


*/

#include "config.h"
#include <stdio.h>
#include <unistd.h>
#include <regex.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include "jargrep.h"
#include "jartool.h"
#include "pushback.h"
#include "zipfile.h"
#include "zlib.h"
#include "compress.h"
#include <getopt.h>

void version(void);
void help(const char *name);

#define Usage "Usage: %s [-bcinsVw] [--version|--help] <-e PATTERN | PATTERN> FILE ...\n"

/*
Function name: opt_valid
arg:	options	Bitfield flag that contains the command line options of grepjar.
purpose:	To guard agains the occurance of certain incompatible flags being used
together.
returns: TRUE if options are valid, FALSE otherwise.
*/

static int opt_valid(int options) {
	int retflag;

	if((options & JG_PRINT_COUNT) && 
		(options & (JG_PRINT_BYTEOFFSET | JG_PRINT_LINE_NUMBER)))
	{
		retflag = FALSE;
	}
	else retflag = TRUE;

	return retflag;
}

/*
Function name: create_regexp
args:	regstr	String containing the uncompiled regular expression.  This may be the 
				expression as is passed in through argv.
		options	This is the flag containing the commandline options that have been
				parsed by getopt.
purpose: Handle the exception handling involved with setting upt a new regular 
expression.
returns: Newly allocated compile regular expression ready to be used in an regexec call.
*/

static regex_t *create_regexp(const char *regstr, int options) {
	regex_t *exp;
	int errcode;
	int msgsize;
	char *errmsg;

	if((exp = (regex_t *) malloc(sizeof(regex_t))))
	{
		if((errcode = regcomp(exp, regstr, (options & JG_IGNORE_CASE) ? REG_ICASE : 0))) {
			fprintf(stderr, "regcomp of regex failed,\n");
			if((errmsg = (char *) malloc(msgsize = regerror(errcode, exp, NULL, 0) + 1))) {
				regerror(errcode, exp, errmsg, msgsize);
				fprintf(stderr, "Error: %s\n", errmsg);
				free(exp);
				free(errmsg);
				exit(1);
			}
			else {
				fprintf(stderr, "Malloc of errmsg failed.\n");
				fprintf(stderr, "Error: %s\n", strerror(errno));
				free(exp);
				exit(1);
			}
		}
	}
	else {
		fprintf(stderr, "Malloc of regex failed,\n");
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(1);
	}

	return exp; 
}

/*
Function name: check_sig
args:	scratch	Pointer to array of bytes containing signature.
		pbf		Pointer to push back handle for jar file.
purpose: 	Verify that checksum is correct.
returns: 0, 1, or 2.  0 means we are ready to read embedded file information.  1 means
we have read beyound the embedded file list and can exit knowing we have read all the
relevent information.  2 means we still haven't reached embdedded file list and need to
do some more reading.
*/
static int check_sig(ub1 *scratch, pb_file *pbfp) {
	ub4 signature;
	int retflag = 0;

	signature = UNPACK_UB4(scratch, 0);

#ifdef DEBUG    
    printf("signature is %x\n", signature);
#endif
    if(signature == 0x08074b50){
#ifdef DEBUG    
      printf("skipping data descriptor\n");
#endif
      pb_read(pbfp, scratch, 12);
      retflag = 2;
    } else if(signature == 0x02014b50){
#ifdef DEBUG    
      printf("Central header reached.. we're all done!\n");
#endif
      retflag = 1;
    }else if(signature != 0x04034b50){
      printf("Ick! %#x\n", signature);
      retflag = 1;
    }
    
	return retflag;
}

/*
Function name: decd_siz
args	csize		Pointer to embedded file's compressed size.
		usize		Pointer to embedded file's uncmpressed size.
		fnlen		Pointer to embedded file's file name length.
		elfen		Pointer to length of extra fields in jar file.
		flags		Pointer to bitmapped flags.
		method		Pointer to indicator of storage method of embedded file.
		file_header	Pointer to string containing the above values to be unbacked.
Purpose: Unpack the series of values from file_header.
*/

static void decd_siz(ub4 *csize, ub4 *usize, ub2 *fnlen, ub2 *eflen, ub2 *flags, ub2 *method, ub1 *file_header) {
    *csize = UNPACK_UB4(file_header, LOC_CSIZE);
#ifdef DEBUG    
    printf("Compressed size is %u\n", *csize);
#endif

	*usize = UNPACK_UB4(file_header, LOC_USIZE);
#ifdef DEBUG
	printf("Uncompressed size is %u\n", *usize);
#endif

    *fnlen = UNPACK_UB2(file_header, LOC_FNLEN);
#ifdef DEBUG    
    printf("Filename length is %hu\n", *fnlen);
#endif

    *eflen = UNPACK_UB2(file_header, LOC_EFLEN);
#ifdef DEBUG    
    printf("Extra field length is %hu\n", *eflen);
#endif

    *flags = UNPACK_UB2(file_header, LOC_EXTRA);
#ifdef DEBUG    
    printf("Flags are %#hx\n", *flags);
#endif

    *method = UNPACK_UB2(file_header, LOC_COMP);
#ifdef DEBUG
    printf("Compression method is %#hx\n", *method);
#endif

}

/*
Function name: new_filename
args:	pbf		Pointer to push back file handle.  Used for reading input file.
		len		Length of file name to be read.
purpose:	Read in the embedded file name from jar file.
returns: Pointer to newly allocated string containing file name.
*/

static char *new_filename(pb_file *pbf, ub4 len) {
	char *filename;

	if(!(filename = (char *) malloc(len + 1))) {
		fprintf(stderr, "Malloc failed of filename\n");
		fprintf(stderr, "Error: %s\n", strerror(errno));
	}
    pb_read(pbf, filename, len);
    filename[len] = '\0';

#ifdef DEBUG    
    printf("filename is %s\n", filename);
#endif

	return filename;
}

/*
Funtion name: read_string
args:	pbf		Pointer to push back file handle.  Used for reading input file.
		size	Size of embedded file in bytes.
purpose:	Create a string containing the contents of the embedded noncompressed file.
returns: Pointer to newly allocated string containing embedded file contents.
*/

static char *read_string(pb_file *pbf, int size) {
	char *page;
	
	if((page = (char *) malloc(size + 1))) {
		pb_read(pbf, page, size);
		page[size] = '\0';
	}
	else {
		fprintf(stderr, "Malloc of page buffer failed.\n");
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(1);
	}

	return page;
}

/*
Function name: extract_line
args:	stream	String containing the full contents of a file which is to be substringed
				in order to provide line representing our grep output.
		begin	Index into stream which regular expression first matches.
		end		Index into stream which end of match to the regular expression.
		b		Pointer to the index of what will be the beginning of the line when
				string is returned.  Used for -b option.
purpose:	Create a string that can be printed by jargrep from the long string stream.
The matching line that is printed out by jargrep is generated by this function.
returns: Pointer to newly allocated string containing matched expression.
*/

static char *extract_line(const char *stream, regoff_t begin, regoff_t end, int *b) {
	int e;
	int length;
	char *retstr;

	for(*b = begin; *b >= 0 && !iscntrl((unsigned char)stream[*b]); (*b)--);
	(*b)++;
	for(e = end; stream[e] == '\t' || !iscntrl((unsigned char)stream[e]); e++);
	length = e - *b;
	if((retstr = (char *) malloc(length + 1))) {
		sprintf(retstr, "%d:", *b);
		strncpy(retstr, &(stream[*b]), length);
		retstr[length] = '\0';
	}
	else {
		fprintf(stderr, "Malloc failed of output string.\n");
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(1);
	}

	return retstr;
}

/*
Function name: chk_wrd
args:	exp		Pointer to compiled POSIX style regular expression of search target.
		str		String known to contain at least one match of exp.
purpose: Verify that the occurance of the regular expression in str occurs as a whole
word and not a substring of another word.
returns: TRUE if it is a word, FALSE of it is a substring.
*/

static int chk_wrd(regex_t *exp, const char *str) {
	int wrd_fnd = FALSE;
	int regflag;
	int frnt_ok;
	int bck_ok;
	const char *str2;
	regmatch_t match;

	str2 = str;
	frnt_ok = bck_ok = FALSE;
	while(!wrd_fnd && !(regflag = regexec(exp, str2, 1, &match, 0))) {
		if(!match.rm_so && (str2 == str)) frnt_ok = TRUE;
		else if(!isalnum((unsigned char)str2[match.rm_so - 1])
			&& str2[match.rm_so - 1] != '_')
			frnt_ok = TRUE;
		else frnt_ok = FALSE;
		if(frnt_ok) {
			if(str2[match.rm_eo] == '\0') bck_ok = TRUE;
			else if(!isalnum((unsigned char)str2[match.rm_eo])
				&& str2[match.rm_eo] != '_')
				bck_ok = TRUE;
			else bck_ok = FALSE;
		}
		wrd_fnd = frnt_ok && bck_ok;
		str2 = &(str2[match.rm_eo]);
	}

	return wrd_fnd;
}

/*
Function name: prnt_mtchs
args:	exp			Pointer to compiled POSIX style regular expression of search target.
		filename	String containing the name of the embedded file which matches have
					been found in.
		stream		String containing the processed contents of the embedded jar file
					represended with filename.
		pmatch		Array of regmatch_t matches into stream.
		nl_offset	Array of offsets of '\n' characters in stream.  May be NULL if -n is
					not set on command line.
		num			Number of matches in pmatch array.
		lines		Number of lines in file.  Not set if -n is not set on command line.
		options		Bitwise flag containing flags set to represent the command line 
					options.
purpose:	Control output of jargrep.  Output is controlled by which options have been
set at the command line.
*/

static void prnt_mtchs(regex_t *exp, const char *filename, const char *stream, regmatch_t *pmatch, regmatch_t *nl_offset, int num, int lines, int options) {
	int i;
	int j = 0;
	int ln_cnt;
	int begin;
	int o_begin;
	char *str;

	o_begin = -1;
	ln_cnt = 0;
	for(i = 0; i < num; i++) {
		str = extract_line(stream, pmatch[i].rm_so, pmatch[i].rm_eo, &begin);
		if(begin > o_begin) {
			if(!(options & JG_WORD_EXPRESSIONS) || chk_wrd(exp, str)) {
				ln_cnt++;
				if(!(options & JG_PRINT_COUNT)) {
					printf("%s:", filename);
					if(options & JG_PRINT_LINE_NUMBER) {
						for(; j < lines && nl_offset[j].rm_so < begin; j++);
						printf("%d:", j + 1);
					}
					if(options & JG_PRINT_BYTEOFFSET) printf("%d:", begin);
					printf("%s\n", str);
				}
			}
		}
		o_begin = begin;
		free(str);
	}
	if(options & JG_PRINT_COUNT) printf("%s:%d\n", filename, ln_cnt);
}

/*
Function name: check_crc
args:	pbf		Pointer to pushback file pointer for jar file.
		stream	String containing the non modified contents fo the extraced file entry.
		usize	Size of file in bytes.
purpose:	Verify the CRC matches that as what is stored in the jar file.
*/

static void check_crc(pb_file *pbf, const char *stream, ub4 usize) {
	ub4 crc=0;
	ub4 lcrc;
	ub1 scratch[16];

	crc = crc32(crc, NULL, 0);
	crc = crc32(crc, (const unsigned char *)stream, usize);
	if(pb_read(pbf, scratch, 16) != 16) {
		perror("read");
        exit(1);
	}
	if(UNPACK_UB4(scratch, 0) != 0x08074b50) {
		fprintf(stderr, "Error! Missing data descriptor!\n");
		exit(1);
	}
	lcrc = UNPACK_UB4(scratch, 4);
	if(crc != lcrc){
    	fprintf(stderr, "Error! CRCs do not match! Got %x, expected %x\n",
              crc, lcrc);
      	exit(1);
    }
}

/*
Function name mk_ascii
args:	stream	String that contains the contents of the extraced file entry.
		usize	String size.
purpose:	Make certain that the contents of the file are ASCII, not binary.  This
permits grepping of binary files as well by converting non ASCII and control characters
into '\n'.
*/

static void mk_ascii(char *stream, size_t usize) {
	size_t i;

	for(i = 0; i < usize; i++) 
		if(stream[i] != '\t'
		   && (iscntrl((unsigned char)stream[i])
		       || (unsigned char) stream[i] >= 128))
			stream[i] = '\n';
}

/*
Funtion name: fnd_match
args:	exp			Pointer to compiled POSIX style regular expression of search target.
		str_stream	String that contains the contents of the extracted file entry.
		i			Pointer to counter and index of matches.
purpose:	Search str_stream for occurances of the regular expression exp and create
an array of matches.
returns:  Pointer to newly allocated array of regmatch_t which gives indexes to start
and end of matches.  NULL is returned upon no matches found.
*/

static regmatch_t *fnd_match(regex_t *exp, const char *str_stream, int *i) {
	int regflag;
	regmatch_t match;
	regmatch_t *match_array;
	regmatch_t *tmp;

	match_array = NULL;
	for(*i = 0, regflag = regexec(exp, str_stream, 1, &match, 0); !regflag; 
		regflag = regexec(exp, &(str_stream[match.rm_eo]), 1, &match, 0), (*i)++)
	{
		if((tmp = (regmatch_t *) 
		    realloc(match_array, sizeof(regmatch_t) * ((*i) + 1))))
		{
			match_array = tmp;
			if(*i) {
				match.rm_so += match_array[(*i) - 1].rm_eo;
				match.rm_eo += match_array[(*i) - 1].rm_eo;
			}
			match_array[*i] = match;
		}
		else {
			fprintf(stderr, "Realloc of match_array failed.\n");
			fprintf(stderr, "Error: %s\n", strerror(errno));
			exit(1);
		}
	} 

	return match_array;
}

/*
Function name: cont_grep
args:	exp		Pointer to compiled POSIX style regular expression of search target.
		nl_exp	Pointer to compiled POSIX style regular expression of newlines.  This
				argument is NULL unless the -n option is used on the command line.
		fd		File descriptor of the jar file being grepped.
		pbf		Pointer to pushback file style file stream.  This is for use with
				the pushback.c file io funtions.
		options	Bitwise flag containing flags set to represent the command line options.
purpose:	This function handles single entries in an open jar file.  The header is
read and then the embeded file is extracted and grepped.
returns: FALSE upon failure, TRUE otherwise.
*/

static int cont_grep(regex_t *exp, regex_t *nl_exp, int fd, pb_file *pbf, int options) {
	int retflag = TRUE;
	int i;
	int j;
	ub4 csize;
	ub4 usize;
	ub2 fnlen;
	ub2 eflen;
	ub2 flags;
	ub2 method;
	ub1 file_header[30];
	char *filename;
	char *str_stream;
	regmatch_t *match_array;
	regmatch_t *nl_offsets=0;

	if(pb_read(pbf, (file_header + 4), 26) != 26) {
		perror("read");
		retflag = FALSE;
   	}
	else {
		decd_siz(&csize, &usize, &fnlen, &eflen, &flags, &method, file_header);
		filename = new_filename(pbf, fnlen);
		lseek(fd, eflen, SEEK_CUR);
		if(filename[fnlen - 1] != '/') {
			str_stream = (method == 8 || (flags & 0x0008)) ? 
				(char *) inflate_string(pbf, &csize, &usize) : 
					read_string(pbf, csize);
			if(flags & 0x008) check_crc(pbf, str_stream, usize);
			mk_ascii(str_stream, usize);
			match_array = fnd_match(exp, str_stream, &i);
			if((options & JG_PRINT_LINE_NUMBER) && i) 
				nl_offsets = fnd_match(nl_exp, str_stream, &j);
			prnt_mtchs(exp, filename, str_stream, match_array, nl_offsets, i, j, options);
			if(match_array) free(match_array);
			free(str_stream);
		}
		free(filename);
		retflag = TRUE;
	}

	return retflag;
}

/*
Funtion name: jargrep
args:	exp		Pointer to compiled POSIX style regular expression of search target.
		nl_exp	Pointer to compiled regular expression for newlines or NULL.  Only set 
				if -n option is present at command line.
		jarfile	Filename of jar file to be searched.
		options	Bitwise flag containing flags set to represent the command line options.
purpose:	Open jar file.  Check signatures.  When right signature is found go to deeper
grep routine.
*/

static void jargrep(regex_t *exp, regex_t *nl_exp, const char *jarfile, int options){
	int fd;
	int floop = TRUE;
	pb_file pbf;
	ub1 scratch[16];

	if((fd = open(jarfile, O_RDONLY)) == -1) {
		if(!(options & JG_SUPRESS_ERROR))
			fprintf(stderr, "Error reading file '%s': %s\n", jarfile, strerror(errno));
	}
	else {
		pb_init(&pbf, fd);	
		
		do {
			if(pb_read(&pbf, scratch, 4) != 4) {
				perror("read");
				floop = FALSE;
			}
			else {
				switch (check_sig(scratch, &pbf)) {
				case 0:
					floop = cont_grep(exp, nl_exp, fd, &pbf, options);
					break;
				case 1:
					floop = FALSE;
					break;
				case 2:
					/* fall through continue */
					;
				}
			}
		} while(floop);
	}
}

/* This is used to mark options with no short value.  */
#define LONG_OPT(Num)  ((Num) + 128)

#define OPT_HELP     LONG_OPT (0)

static const struct option option_vec[] =
{
  { "help", no_argument, NULL, OPT_HELP },
  { "version", no_argument, NULL, 'V' },
  { NULL, no_argument, NULL, 0 }
};

/*
Funtion Name: main
args:	argc	number of in coming args.
		argv	array of strings.
purpose: Entry point of the program.  Parse command line arguments and set options.
Set up regular expressions.  Call grep routines for each file as input.
returns: 1 on error 0 on success.
*/

int main(int argc, char **argv) {
	int c;
	int retval = 0;
	int fileindex;
	int options = 0;
	regex_t *regexp;
	regex_t *nl_exp = NULL;
	char *regexpstr = NULL;

	while((c = getopt_long(argc, argv, "bce:insVw",
			       option_vec, NULL)) != -1) {
		switch(c) {
			case 'b':
				options |= JG_PRINT_BYTEOFFSET;
				break;
			case 'c':
				options |= JG_PRINT_COUNT;
				break;
			case 'e':
				if(!(regexpstr = (char *) malloc(strlen(optarg) + 1))) {
					fprintf(stderr, "Malloc failure.\n");
					fprintf(stderr, "Error: %s\n", strerror(errno));
					exit(1);
				}
				strcpy(regexpstr, optarg);
				break;
			case 'i':
				options |= JG_IGNORE_CASE;
				break;
			case 'n':
				options |= JG_PRINT_LINE_NUMBER;
				break;
			case 's':
				options |= JG_SUPRESS_ERROR;
				break;
			case 'v':
				options |= JG_INVERT;
				break;
			case 'V':
				version ();
				break;
			case 'w':
				options |= JG_WORD_EXPRESSIONS;
				break;
			case OPT_HELP:
				help(argv[0]);
				break;
			default:
				fprintf(stderr, Usage, argv[0]);
				exit(1);
		}
	}
	if(!regexpstr){
		if(((argc - optind) >= 2)) {
			regexpstr = argv[optind];
			fileindex = optind + 1;
		}
		else {
			fprintf(stderr, "Invalid arguments.\n");
			fprintf(stderr, Usage, argv[0]);
			exit(1);
		}
	}
	else if((argc - optind) == 1) {
		fileindex = optind;
	}
	else {
		fprintf(stderr, "Invalid arguments.\n");
		fprintf(stderr, Usage, argv[0]);
		exit(1);
	}

	if(opt_valid(options)) {
		regexp = create_regexp(regexpstr, options);
		if(options & JG_PRINT_LINE_NUMBER) nl_exp = create_regexp("\n", 0);
		init_inflation();
		for(; fileindex < argc; fileindex++)
			jargrep(regexp, nl_exp, argv[fileindex], options);
		regfree(regexp);
		if(options & JG_PRINT_LINE_NUMBER) regfree(nl_exp);
	}
	else {
		retval = 1;
		fprintf(stderr, "Error: Invalid combination of options.\n");
	}

	return retval;
}

void help(const char *filename)
{
  printf (Usage, filename);
  printf ("\
\n\
Search files in a jar file for a pattern.\n\
\n\
   -b                print byte offset of match\n\
   -c                print number of matches\n\
   -i                compare case-insensitively\n\
   -n                print line number of each match\n\
   -s                suppress error messages\n\
   -w                force PATTERN to match only whole words\n\
   -e PATTERN        use PATTERN as regular expression\n\
   -V|--version      print version number and exit\n\
   --help            print help\n\
");

  exit (0);
}

void version ()
{
  printf("grepjar (%s) %s\n\n", PACKAGE, VERSION);
  printf("Copyright 1999, 2000, 2001  Bryan Burns\n");
  printf("Copyright 2000 Cory Hollingsworth\n");
  printf("Copyright 2002 Free Software Foundation\n");
  printf("\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
  exit (0);
}
