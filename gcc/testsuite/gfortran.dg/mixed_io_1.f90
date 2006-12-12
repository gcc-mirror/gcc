! { dg-do run }
! { dg-additional-sources mixed_io_1.c }
! { dg-options "-w" } 
      call cio
      write(*,"(A)") '6789' ! { dg-output "123456789" }
      end
