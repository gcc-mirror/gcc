! { dg-do run }
! PR34782 tab format failure to display properly (regression vs. g77)
      character a(6)
      character(22)  :: output
      data a / 'a', 'b', 'c', 'd', 'e', 'f' /
      !write(*,'(a)') "123456789012345678901234567890"
      write(output,'(T20,A3,  T1,A4,  T5,A2,  T7,A2,  T9,A4, T17,A2)')
     1 'a', 'b', 'c', 'd', 'e', 'f' 
      if (output .ne. "   b c d   e     f   a") call abort
      end
