/*
 * dirent.h
 */

#ifndef _DIRENT_H
#  define _DIRENT_H

#  include <sys/types.h>
#  include <limits.h>

#define MAXNAMLEN	255	/* maximum filename length		*/

#ifndef NAME_MAX
#define	NAME_MAX	(MAXNAMLEN - 1)
#endif

struct dirent			/* data from getdents()/readdir()	*/
{
    ino_t	d_ino;		/* inode number of entry		*/
    off_t	d_off;		/* offset of disk directory entry	*/
    wchar_t	d_reclen;	/* length of this record		*/
    char	d_name[MAXNAMLEN + 1];
};


/* The following nonportable ugliness could have been avoided by defining
 * DIRENTSIZ and DIRENTBASESIZ to also have (struct dirent *) arguments.
 * There shouldn't be any problem if you avoid using the DIRENTSIZ() macro.
 */

#define	DIRENTBASESIZ		(((struct dirent *)0)->d_name \
				- (char *)&((struct dirent *)0)->d_ino)

#define	DIRENTSIZ(namlen)	((DIRENTBASESIZ + sizeof(long) + (namlen)) \
				/ sizeof(long) * sizeof(long))



#  ifndef _BOOL_T_DEFINED
typedef unsigned char	bool;
#  define _BOOL_T_DEFINED
#  endif

#  define DIRBUF	8192	/* buffer size for fs-indep. dirs	*/
				/* must in general be larger than the	*/
				/* filesystem buffer size		*/

struct _dircontents {
    char		*_d_entry;
    struct _dircontents	*_d_next;
};

typedef struct _dirdesc {
    int			dd_id;	/* uniquely identify each open directory */
    long		dd_loc;	/* where we are in directory entry is this */
    struct _dircontents	*dd_contents;	/* pointer to contents of dir	*/
    struct _dircontents	*dd_cp;		/* pointer to current position	*/
} DIR;


#if defined (__STDC__)
#  define _PROTO(p)	p
#else
#  define _PROTO(p)	()
#  undef  const
#  undef  volatile
#endif

/* Functions */

extern DIR *		opendir	_PROTO ((const char *));
extern struct dirent * 	readdir _PROTO ((DIR *));
extern void		rewinddir _PROTO ((DIR *));

extern int		closedir _PROTO ((DIR *));
extern void		seekdir	_PROTO ((DIR *, off_t));
extern off_t		telldir	_PROTO ((DIR *));

extern int		chdir _PROTO ((const char *));
extern char * 		getcwd _PROTO ((char *, size_t));

extern int		mkdir _PROTO ((const char *));

extern int		rmdir _PROTO ((const char *));
extern int		scandir _PROTO ((char *,
			 struct dirent ***,
			 int (*)(const void *, const void *),
			 int (*)(const void *, const void *)));

extern int		_chdrive _PROTO ((int));
extern int		_getdrive _PROTO ((void));
extern char * 		_getdcwd _PROTO ((int, char *, int));

extern bool		IsHPFSFileSystem _PROTO ((char *));

#endif
