/* PR sanitizer/65367 */
/* { dg-do compile } */
/* { dg-options "-fno-tree-ccp -fno-tree-copy-prop -fno-tree-dominator-opts -fno-tree-fre -fsanitize=object-size" } */

int
foo (char *p)
{
  return *((const char *) "") - *p;
}
