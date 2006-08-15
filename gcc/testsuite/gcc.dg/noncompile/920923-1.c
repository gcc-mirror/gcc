typedef BYTE unsigned char;	/* { dg-error "expected" } */
typedef int item_n;
typedef int perm_set;
struct PENT { caddr_t v_addr; };/* { dg-error "expected" } */
typedef struct PENT prec;
typedef struct PENT *prec_t;
prec_t mem_hash;
BYTE *mem_base;			/* { dg-error "expected" } */
struct PTE {
     BYTE *p_page;		/* { dg-error "expected" } */
     perm_set p_perms;
};
typedef struct PTE pte;
struct PTP {
     union {
	  struct *PTP p_tablep;	/* { dg-error "expected" } */
	  struct *PTE p_entry;
     } u;
     int valid;
};
typedef struct PTP (u.p_tablep);/* { dg-error "expected" } */
int pfree=0;
int pcount=0;

void
mmu_walk_find(va)
caddr_t va;			/* { dg-error "expected" } */
{
     BYTE *page_addr; /* { dg-error "undeclared|for each function" } */
     if (mmu_base[Level1(va)]->valid==0x0) { /* { dg-error "undeclared" } */
	  l1_base = mmu_base[Level1(va)]->(u.p_tablep) = p_alloc(); /* { dg-error "expected|undeclared" } */
	  mmu_base[Level1(va)]->valid = 0x3;
	  for (idx=0; idx<LEVEL1_ENTRIES; idx++) /* { dg-error "undeclared" } */
	       l1_base[idx]->valid = 0x0;
	  goto build_level2;
     } else
	  l1_base = mmu_base[Level1(va)]->(u.p_tablep); /* { dg-error "expected" } */

     if (l1_base[Level2(va)]->valid==0x0) {
     build_level2:
	  l2_base = l1_base[Level2(va)]->(u.p_tablep) = p_alloc(); /* { dg-error "expected|undeclared" } */
	  l1_base[Level2(va)]->valid = 0x3;
	  for (idx=0; idx<LEVEL2_ENTRIES; idx++) /* { dg-error "undeclared" } */
	       l2_base[idx]->valid=0x0;
	  goto build_page; /* { dg-error "used but not defined" } */
     } else
	  l2_base = mmu_base[Level2(va)]->(u.p_tablep); /* { dg-error "expected" } */

     page_addr = l2_base[Level2(va)]->valid;
}

void *
a_translate(va_op, v_addr)
int va_op;
caddr_t v_addr;			/* { dg-error "expected" } */
{
     register prec_t bucket;
     register caddr_t p_addr;	/* { dg-error "expected|undeclared" } */
     bucket = mem_hash+((((v_addr)>>ITEMBITS))&hash_mask);  /* { dg-error "undeclared" } */
     do {
	  if (bucket->v_addr == ((v_addr)>>ITEMBITS) {	/* { dg-error "expected|undeclared|no member" } */
	       if(!(bucket->perm_set&va_op))
		    goto prot_fault;
	       return mem_base + v_addr;
	  }
     } while((bucket++)->v_addr != ((caddr_t)0));  /* { dg-error "expected|undeclared|no member" } */

 page_miss:
     p_addr = (--bucket)->p_addr;	/* { dg-error "no member" } */
 page_type:
     switch (p_addr) {
     case BUCKET_FULL:		/* { dg-error "undeclared" } */
	  enlarge_hash_table(mem_hash);
     case((caddr_t)0):		/* { dg-error "expected" } */
	  p_addr = fill_item_entry(va_op, v_addr);
	  goto page_type;
     case((caddr_t)1):		/* { dg-error "expected" } */
     default:
	  ((void)(((0))?0:(__eprintf("Failed assertion`%s'at line%d of`%s'.\n",
				     "FALSE", 327, "b.c"), 0)));
     }
}

void
flush_hash(hasht, hash_size)
prec_t hasht;
int hash_size;
{
     register prec_t bucket;
     register int idx;
     bucket = hasht;
     for(idx=(hash_size*3)-1; idx>=0; idx--) {
	  bucket->v_addr = ((caddr_t)0);/* { dg-error "undeclared|expected|no member" } */
	  bucket->p_addr = ((caddr_t)0);/* { dg-error "no member|expected" } */
	  bucket->perm_set = VA_EMPTY;	/* { dg-error "undeclared|no member" } */
     }
}

extern void *calloc(__SIZE_TYPE__, __SIZE_TYPE__);

void
init_mem()
{
     mem_base = (BYTE *) calloc(1024, (1<<13)); /* { dg-error "undeclared|expected" } */
     ((void)((mem_base != (BYTE *)0)	/* { dg-error "expected" } */
	     ? 0
	     : (__eprintf("Failed assertion`%s'at line%d of`%s'.\n",
			  "mem_base != (BYTE *)0", 366, "b.c"),
		0)));
     hash_num = INIT_NUM_ENTRIES * 3;	/* { dg-error "undeclared" } */
     mem_hash = (prec_t) calloc(hash_num, sizeof(prec));
     ((void)((mem_hash != (prec_t)0)
	     ? 0
	     : (__eprintf("Failed assertion`%s'at line%d of`%s'.\n",
			  "mem_hash != (prec_t)0", 370, "b.c"),
		0)));
     flush_hash(mem_hash, 32);
     build_ptables(mem_base, 1024*(1<<13)); /* { dg-bogus "integer overflow" "int smaller than 32 bits" } */
}

struct tm {
     int tm_sec; int tm_min; int tm_hour;
     int tm_mday;int tm_mon; int tm_year;
     int tm_wday;int tm_yday;int tm_isdst;
     char*tm_zone; long tm_gmtoff;
};
