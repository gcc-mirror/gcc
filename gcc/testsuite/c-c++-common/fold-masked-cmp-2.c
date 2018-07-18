/* Based on PR 67328 */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

enum output_type
{
  type_pde,
  type_relocatable,
  type_pie,
  type_dll,
};

struct bfd_link_info
{
  enum output_type type : 2;
  unsigned int pad : 30;
};

#define bfd_link_pde(info)	   ((info)->type == type_pde)
#define bfd_link_dll(info)	   ((info)->type == type_dll)
#define bfd_link_relocatable(info) ((info)->type == type_relocatable)
#define bfd_link_pie(info)	   ((info)->type == type_pie)
#define bfd_link_executable(info)  (bfd_link_pde (info) || bfd_link_pie (info))
#define bfd_link_pic(info)	   (bfd_link_dll (info) || bfd_link_pie (info))

int result;

void test_pic (struct bfd_link_info *info)
{
  if (bfd_link_pic (info))
    result++;
}

int test_exe (struct bfd_link_info *info)
{
  if (bfd_link_executable (info))
    result++;

  return 0;
}

/* { dg-final { scan-assembler-times "testn?b" 2 } } */

