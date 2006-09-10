! { dg-do compile }
! Tests the fix for PR28923 in which initializer array constructors with
! a missing initial array index and negative stride would be incorrectly
! interpreted.
!
! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr>
!
real, dimension(3,3), parameter :: a=reshape ((/(i, i = 1,9)/),(/3,3/))
real, dimension(2,3) :: b=a(:2:-1,:)  ! { dg-error "different shape for Array assignment" }
real, dimension(2,3) :: c=a(3:2:-1,:)
print *, b
print *, c
end

