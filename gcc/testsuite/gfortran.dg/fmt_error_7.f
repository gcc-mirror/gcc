! { dg-do compile }
! PR37446 Diagnostic of edit descriptors, esp. EN
      character(40) :: fmt_string
      write(*, '(1P,2E12.4)') 1.0
      write(*,'(EN)') 5.0 ! { dg-error "Positive width required" }
      write(*,'("abcdefg",EN6,"hjjklmnop")') 5.0 ! { dg-error "Period required" }
      end
