! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
! PR 69742 - this used to ICE with front-end optimizatoin
! Original test case by Marco Restelli.
program p
 implicit none
 integer, allocatable :: i(:), j

  allocate( i(5) )
  i = (/( j , j=1,5 )/)

  ! The ICE appears when "size(i)" is used twice in associate
  associate( i5 => i(size(i):size(i)) ) ! this gives ICE
  !associate( i5 => i(size(2*i):size(i)) ) ! this works
  i5 = 2
  end associate

  write(*,*) i
end program p
