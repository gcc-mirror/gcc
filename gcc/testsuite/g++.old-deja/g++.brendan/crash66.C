// Build don't link: 
// Special g++ Options: -O
// GROUPS passed old-abort
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned long ulong;
extern int swap_endian;			 
inline ushort
swapshort(ushort value)
{
    value &= 0xffff;
    return ((value << 8) | (value >> 8));
}
struct eshort
{
    ushort	data;			 
    operator ushort() { return swap_endian ? swapshort(data) : data;}
    eshort(ushort t) { data = swap_endian ? swapshort(t) : t;}
    eshort() {}
};
inline ulong
swaplong(ulong value)
{
    ulong v = (value << 16) | (value >> 16);
    return ((v >> 8) & 0x00ff00ff) | ((v << 8) & 0xff00ff00);
};
struct elong
{
    ulong	data;			 
    operator ulong() { return swap_endian ? swaplong(data) : data;}
    elong(ulong t) { data = swap_endian ? swaplong(t) : t; }
    elong() {}
};
struct digiheader
{
    uchar	type[2];		 
    eshort	soft_version;		 
    eshort	lo_boot_rev;		 
    eshort	hi_boot_rev;		 
    eshort	load_segment;		 
    eshort	length;			 
    eshort	exec_start;		 
    eshort	image_offset;		 
    elong	startup_code[2];	 
    elong	checksum;		 
};
extern void uncompress(uchar* buf, ulong len);
extern ulong compress(char* filename, uchar* buffer, ulong);
struct filehdr
{
    eshort	f_magic;		 
    eshort	f_nscns;		 
    elong	f_timdat;		 
    elong	f_symptr;		 
    elong	f_nsyms;		 
    eshort	f_opthdr;		 
    eshort	f_flags;		 
};
struct aouthdr
{
    eshort	magic;			 
    eshort	vstamp;			 
    elong	tsize;			 
    elong	dsize;			 
    elong	bsize;			 
    elong	entry;			 
    elong	text_start;		 
    elong	data_start;		 
    elong	bss_start;		 
    elong	gprmask;		 
    elong	cprmask[4];		 
    elong	gp_value;		 
};
struct scnhdr
{
    char	s_name[8];		 
    elong	s_paddr;		 
    elong	s_vaddr;		 
    elong	s_size;			 
    elong	s_scnptr;		 
    elong	s_relptr;		 
    elong	s_lnnoptr;		 
    eshort	s_nreloc;		 
    eshort	s_nlnno;		 
    elong	s_flags;		 
};
int file_little_endian;			 
int host_little_endian;			 
int swap_endian;			 
int docheck;				 
int expand;				 
ulong memsize;				 
ulong compression_quality;		 
char *compressfile;			 
int debug_level;			 
extern "C" int getopt (int, char**, char*);
int
main(int argc, char** argv)
{
    uchar checksum;
    uchar docrc;
    ulong len;
    ulong maxlen;
    int i;
    int c;
    int magic;
    int tsize;
    int dsize;
    int quality;
    char dummy;
    uchar* code;
    uchar* buf;
    char* ap;
    digiheader *dh;
    compression_quality = 10000;
    docheck = 0;
    while ((c = getopt(argc, argv, "Ccdf:k:q:x:")) != -1)
    {
	switch (c)
	{
	default:
	    goto usage;
	}
    }
    if ((expand && (docheck || compressfile || quality)) ||
	(quality && !compressfile))
    {
    usage:
	return(2);
    }
    if (compressfile)
    {
	dh->image_offset = len;
	
	len += compress(compressfile, code + len, maxlen - len);
    }
}
