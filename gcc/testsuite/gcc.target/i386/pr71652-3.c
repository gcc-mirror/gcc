/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=haswell" } */

#pragma GCC push_options
#pragma GCC target ("arch=geode") /* { dg-error "CPU you selected does not support x86-64 instruction set" } */

__attribute__((constructor)) void foo()
{
  asm ("");
}

#pragma GCC pop_options

int main() { return 0; }
