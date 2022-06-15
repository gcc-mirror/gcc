! { dg-do compile }
! PR fortran/105986
! Contributed by G.Steinmetz

program p
  integer :: i
  logical, parameter :: a(*) = [(btest(8_4,i), i=-1,-1)] ! { dg-error "nonnegative" }
  integer, parameter :: b(*) = [(ibclr(8_4,i), i=-1,-1)] ! { dg-error "nonnegative" }
  integer, parameter :: c(*) = [(ibset(8_4,i), i=-1,-1)] ! { dg-error "nonnegative" }
  logical, parameter :: d(*) = [(btest(8_1,i), i= 8, 8)] ! { dg-error "must be less" }
  integer, parameter :: e(*) = [(ibclr(8_2,i), i=16,16)] ! { dg-error "must be less" }
  integer, parameter :: f(*) = [(ibset(8_4,i), i=32,32)] ! { dg-error "must be less" }
  integer, parameter :: g(*) = [(ibits(8_4,i,1),i=-1,-1)] ! { dg-error "nonnegative" }
  integer, parameter :: h(*) = [(ibits(8_4,1,i),i=-1,-1)] ! { dg-error "nonnegative" }
  integer, parameter :: j(*) = [(ibits(8_4,i,i),i=32,32)] ! { dg-error "must be less" }
end
