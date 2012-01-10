// { dg-do compile }
// { dg-options "-fgnu-tm -fnon-call-exceptions" }

struct S
{
  S ()
  {
  }
};

__attribute__ ((transaction_callable))
void foo (int *p)
{
  S s;
  if (*p)
    ;
}

// { dg-message "sorry, unimplemented: transactional memory is not supported with non-call exceptions" "-fnon-call-exceptions and -fgnu-tm together" { target *-*-* } 0 }
