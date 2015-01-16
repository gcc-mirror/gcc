/* { dg-do compile }  */

struct unipair 
{
  unsigned short unicode;
  unsigned short fontpos;
};

struct __large_struct
{
  unsigned long buf[100];
};

struct vc_data
{
  unsigned long *vc_uni_pagedir_loc;
};

struct uni_pagedir 
{
  unsigned short **uni_pgdir[32];
};

void con_get_unimap (struct vc_data *vc, unsigned short ct, unsigned short *uct,
		     struct unipair *list)
{
  int i, j, k, ect;
  unsigned short **p1, *p2;
  struct uni_pagedir *p;

  ect = 0;

  if (*vc->vc_uni_pagedir_loc)
  {
    p = (struct uni_pagedir *)*vc->vc_uni_pagedir_loc;
    for (i = 0; i < 32; i++)
    if ((p1 = p->uni_pgdir[i]))
    for (j = 0; j < 32; j++)
    if ((p2 = *(p1++)))
    for (k = 0; k < 64; k++)
    {
      if (*p2 < 512 && ect++ < ct)
      {
	{
	  long __pu_err;
	  __typeof__(*((&list->unicode))) *__pu_addr = ((&list->unicode));
	  __typeof__(*((&list->unicode))) __pu_val =
		((unsigned short)((i<<11)+(j<<6)+k));
	  __pu_err = 0;
	  switch ((sizeof(*(&list->unicode))))
	  {
	    case 1:
		__asm__ __volatile__ (
		"1:\n\t"
			"mov." "b" "	%1, %2\n\t"
		"2:\n"
		".section	.fixup,\"ax\"\n"
		"3:\n\t"
			"mov.l	4f, %0\n\t"
			"jmp	@%0\n\t"
			" mov	%3, %0\n\t"
			".balign	4\n"
		"4:	.long	2b\n\t"
			".previous\n"
		".section	__ex_table,\"a\"\n\t"
			".long	1b, 3b\n\t"
			".previous"
		: "=&r" (__pu_err)
		: "r" (__pu_val), "m" ((*(struct __large_struct *)(__pu_addr))),
		  "i" (-14), "0" (__pu_err) : "memory" );

	      break;

	    case 2:
		__asm__ __volatile__ (
		"1:\n\t"
			"mov." "w" "	%1, %2\n\t"
		"2:\n"
		".section	.fixup,\"ax\"\n"
		"3:\n\t"
			"mov.l	4f, %0\n\t"
			"jmp	@%0\n\t"
			" mov	%3, %0\n\t"
			".balign	4\n"
			"4:	.long	2b\n\t"
			".previous\n"
		".section	__ex_table,\"a\"\n\t"
			".long	1b, 3b\n\t"
			".previous"
		: "=&r" (__pu_err)
		: "r" (__pu_val), "m" ((*(struct __large_struct *)(__pu_addr))),
		  "i" (-14), "0" (__pu_err) : "memory" );
	      break;

	    default:
	      break;
	  }
	}


	{
	  long __pu_err;
	  __typeof__(*((&list->fontpos))) *__pu_addr = ((&list->fontpos));
	  __typeof__(*((&list->fontpos))) __pu_val = ((unsigned short) *p2);
	  __pu_err = 0;
	  switch ((sizeof(*(&list->fontpos))))
	  {
	    case 1:
		__asm__ __volatile__ (
		"1:\n\t"
			"mov." "b" "	%1, %2\n\t"
		"2:\n"
		".section	.fixup,\"ax\"\n"
		"3:\n\t"
			"mov.l	4f, %0\n\t"
			"jmp	@%0\n\t"
			" mov	%3, %0\n\t"
			".balign	4\n"
			"4:	.long	2b\n\t"
			".previous\n"
		".section	__ex_table,\"a\"\n\t"
			".long	1b, 3b\n\t"
			".previous"
		: "=&r" (__pu_err)
		: "r" (__pu_val), "m" ((*(struct __large_struct *)(__pu_addr))),
		  "i" (-14), "0" (__pu_err) : "memory" );
	      break;

	    case 2:
	      __asm__ __volatile__ (
		"1:\n\t"
			"mov." "w" "	%1, %2\n\t"
		"2:\n"
			".section	.fixup,\"ax\"\n"
		"3:\n\t"
			"mov.l	4f, %0\n\t"
			"jmp	@%0\n\t"
			" mov	%3, %0\n\t"
		".balign	4\n"
			"4:	.long	2b\n\t"
		".previous\n"
		".section	__ex_table,\"a\"\n\t"
			".long	1b, 3b\n\t"
			".previous"
		: "=&r" (__pu_err)
		: "r" (__pu_val), "m" ((*(struct __large_struct *)(__pu_addr))),
		  "i" (-14), "0" (__pu_err) : "memory" );
	      break;

	    default:
	      break;
	  }
	}

        list++;
      }
      p2++;
    }
  }
}
