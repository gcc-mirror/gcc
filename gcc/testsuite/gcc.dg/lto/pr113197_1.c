/* { dg-options "-std=gnu17" } */
int a;
void exit_error();
void register_dccp() { exit_error(a); }
