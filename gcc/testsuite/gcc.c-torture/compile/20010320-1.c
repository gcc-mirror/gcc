typedef struct sec { 
const char *name;
int id;
int index;
struct sec *next;
unsigned int flags;
unsigned int user_set_vma : 1;
unsigned int reloc_done : 1;
unsigned int linker_mark : 1;
unsigned int gc_mark : 1;
unsigned int segment_mark : 1;
unsigned long long vma; } asection;
 
static void pe_print_pdata (asection *section)
{
  unsigned long long i;
  unsigned long long start = 0, stop = 0;
  int onaline = (3*8) ;

  for (i = start; i < stop; i += onaline)
    {
      if (i + (3*8)  > stop)
	break;

      f (((unsigned long) (((   i + section->vma  ) >> 32) & 0xffffffff)) , ((unsigned long) (((   i + section->vma  ) & 0xffffffff))) ) ;
    }
}

