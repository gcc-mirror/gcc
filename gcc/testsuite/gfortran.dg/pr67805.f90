! { dg-do compile }
! PR fortran/67805
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
!
subroutine p
   integer, parameter :: n = 1
   integer, parameter :: m(3) = [1, 2, 3]
   character(len=1) s(2)
   s = [character((m(1))) :: 'x', 'y']    ! OK.
   s = [character(m(1)) :: 'x', 'y']      ! OK.
   s = [character(m) :: 'x', 'y']         ! { dg-error "INTEGER expression expected" }
   
   ! The next line should case an error, but causes an ICE. 
   s = [character(m(2:3)) :: 'x', 'y']    ! { dg-error "INTEGER expression expected" }
   
   call foo(s)
   s = [character('') :: 'x', 'y']        ! { dg-error "INTEGER expression expected" }
   s = [character(['']) :: 'x', 'y']      ! { dg-error "INTEGER expression expected" }
   s = [character([.true.]) :: 'x', 'y']  ! { dg-error "INTEGER expression expected" }
   s = [character([.false.]) :: 'x', 'y'] ! { dg-error "INTEGER expression expected" }
   s = [character([1.]) :: 'x', 'y']      ! { dg-error "INTEGER expression expected" }
   s = [character([1d1]) :: 'x', 'y']     ! { dg-error "INTEGER expression expected" }
   s = [character([(0.,1.)]) :: 'x', 'y'] ! { dg-error "INTEGER expression expected" }
   s =  [character(null()) :: 'x', 'y']   ! { dg-error "INTEGER expression expected" }
   call foo(s)
end subroutine p

subroutine q
   print *, '1: ', [character(.true.) :: 'x', 'y']  ! { dg-error "INTEGER expression expected" }
   print *, '2: ', [character(.false.) :: 'x', 'y'] ! { dg-error "INTEGER expression expected" }
   print *, '3: ', [character(1.) :: 'x', 'y']      ! { dg-error "INTEGER expression expected" }
   print *, '4: ', [character(1d1) :: 'x', 'y']     ! { dg-error "INTEGER expression expected" }
   print *, '5: ', [character((0.,1.)) :: 'x', 'y'] ! { dg-error "INTEGER expression expected" }
   print *, '6: ', [character(null()) :: 'x', 'y']  ! { dg-error "INTEGER expression expected" }.
end subroutine q
