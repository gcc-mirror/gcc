/* { dg-do compile } */
/* { dg-options "-O1 -flive-patching -fwhole-program" } */

int main()
{
  return 0;
}

/* { dg-message "'-fwhole-program' is incompatible with '-flive-patching=inline-only-static|inline-clone’" "" {target "*-*-*"} 0 } */
