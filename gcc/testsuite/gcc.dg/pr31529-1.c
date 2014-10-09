/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -fgnu89-inline" } */
/* { dg-additional-sources "pr31529-2.c" } */

int
getline ()
{
}

int main() { return 0; }
