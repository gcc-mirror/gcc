C The preprocessor must not introduce a newline after
C the "a" when ARGUMENTS is expanded.

#define ARGUMENTS a\

      subroutine yada (ARGUMENTS)
      end
