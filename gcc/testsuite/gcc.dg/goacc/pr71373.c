/* Unintentional nested function usage.  */
/* Due to missing right braces '}', the following functions are parsed as
   nested functions.  This ran into an ICE.  */

void foo (void)
{
  #pragma acc parallel
  {
    #pragma acc loop independent
    for (int i = 0; i < 16; i++)
      ;
  // Note right brace '}' commented out here.
  //}
}
void bar (void)
{
}

// Adding right brace '}' here, to make this compile.
}


// ..., and the other way round:

void BAR (void)
{
// Note right brace '}' commented out here.
//}

void FOO (void)
{
  #pragma acc parallel
  {
    #pragma acc loop independent
    for (int i = 0; i < 16; i++)
      ;
  }
}

// Adding right brace '}' here, to make this compile.
}
