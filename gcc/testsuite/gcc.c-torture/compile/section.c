/* { dg-do compile } */

/* { dg-require-effective-target named_sections } */
extern int __attribute__ ((__section__(".init.text"))) elv_register(void)
{
 return 0;
}
extern typeof(elv_register) elv_register;
