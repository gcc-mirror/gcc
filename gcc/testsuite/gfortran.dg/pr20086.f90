! { dg-do run }     
! PR 20086 - Missing characters in output with hollerith strings
      implicit none
      character*80 line
      write(line,2070)
      if (line.ne.'  stiffness reformed for this high step')call abort
      write(line,2090)
      if (line.ne.'  stiffness reformed for hello hello')call abort 
      stop

 2070  format (2x,37hstiffness reformed for this high step)
 2090  format (2x,34hstiffness reformed for hello hello)

      end
