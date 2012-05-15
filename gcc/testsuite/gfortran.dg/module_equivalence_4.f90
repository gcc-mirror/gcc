! { dg-do compile }
! This checks the fix for PR37706 in which the equivalence would be
! inserted into the 'nudata' namespace with the inevitable consequences.
!
! Contributed by Lester Petrie <petrielmjr@ornl.gov>
!
module data_C 
    integer, dimension(200) :: l  = (/(201-i, i = 1,200)/)
    integer :: l0
    integer :: l24, l27, l28, l29
    equivalence ( l(1), l0 )
  end module data_C 

subroutine nudata(nlibe, a, l) 
  USE data_C, only:  l24, l27, l28, l29
  implicit none
  integer  :: nlibe 
  integer  :: l(*) 
  real :: a(*)
  print *, l(1), l(2)
  return  
end subroutine nudata
      
  integer  :: l_(2) = (/1,2/), nlibe_ = 42
  real :: a_(2) = (/1.,2./)  
  call nudata (nlibe_, a_, l_)
end
