/* Extracted from the sdm module in perl.  */

/* { dg-require-stack-size "12*2+8+1024+8" } */

typedef struct {
	char *dptr;
	int dsize;
} datum;
extern long sdbm_hash  (char *, int)  ;
extern void  sdbm__putpair   (char *, datum, datum)  ;
void
sdbm__splpage (char *pag, char *New, long int sbit)
{
	datum key;
	datum val;
	register int n;
	register int off = 1024 ;
	char cur[1024 ];
	register short *ino = (short *) cur;
	(void) __builtin_memcpy(cur, pag, 1024 );
	(void) ({ void *__s = ( pag ); __builtin_memset ( __s , '\0',     1024   ) ; __s; });
	(void) ({ void *__s = ( New ); __builtin_memset ( __s , '\0',     1024   ) ; __s; });
	n = ino[0];
	for (ino++; n > 0; ino += 2) {
		key.dptr = cur + ino[0]; 
		key.dsize = off - ino[0];
		val.dptr = cur + ino[1];
		val.dsize = ino[0] - ino[1];
		(void) sdbm__putpair ((sdbm_hash(( key ).dptr, ( key ).dsize)  & sbit) ? New : pag, key, val);
		off = ino[1];
		n -= 2;
	}
}
