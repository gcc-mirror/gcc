/* { dg-do compile } */

void ldt_add_entry(void)
{
   __asm__ ("" :: "m"(({unsigned __v; __v;})));
}

