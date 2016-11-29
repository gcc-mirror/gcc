/* { dg-do compile } */

#pragma GCC push_options
#pragma GCC target ("arch=generic") /* { dg-error "'generic' CPU can be used only for 'target\\(\"tune=\"\\)' attribute" } */

__attribute__((constructor)) void foo()
{
  asm ("");
}

#pragma GCC pop_options

int main() { return 0; }
