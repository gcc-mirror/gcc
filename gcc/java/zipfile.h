/* Definitions for using a zipped' archive.

   Copyright (C) 1996, 1997, 1998, 1999, 2000  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

struct ZipFile {
  int fd;
  long size;
  long count;
  long dir_size;
  char *central_directory;
};

typedef struct ZipFile ZipFile;

struct ZipDirectory {
  int direntry_size;
  int filename_offset;
  long size; /* length of file */
  long filestart;  /* start of file in archive */
  long filename_length;
  /* char mid_padding[...]; */
  /* char filename[filename_length]; */
  /* char end_padding[...]; */
};

typedef struct ZipDirectory ZipDirectory;

struct ZipFileCache {
  struct ZipFile z;
  struct ZipFileCache *next;
  char *name;
};

extern struct ZipFileCache *SeenZipFiles;

#define ZIPDIR_FILENAME(ZIPD) ((char*)(ZIPD)+(ZIPD)->filename_offset)
#define ZIPDIR_NEXT(ZIPD) \
   ((ZipDirectory*)((char*)(ZIPD)+(ZIPD)->direntry_size))
#define ZIPMAGIC 0x504b0304	

extern ZipFile * opendir_in_zip PARAMS ((const char *, int));
extern int read_zip_archive PARAMS ((ZipFile *));
#ifdef JCF_ZIP
extern int open_in_zip PARAMS ((struct JCF *, const char *,
			       const char *, int));
#endif
