/* { dg-do compile } */
/* { dg-options "-O2" } */

int mt7615_add_interface_dev_0;
int ffs(int x) { asm("" : : "rm"(x)); }
int mt7615_add_interface() { ffs(~mt7615_add_interface_dev_0); }
