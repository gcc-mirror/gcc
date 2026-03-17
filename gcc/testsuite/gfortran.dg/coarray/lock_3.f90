! { dg-do run }
! Test case from PR121360
program memain
   use, intrinsic :: iso_fortran_env, only : lock_type
   type(lock_type), codimension[*] :: lck
   integer, codimension[*] :: count
   integer :: i, j, s
   do j=4,5  ! To be adjusted, dunno how long this runs
     if (this_image() == 1) count = 0
     sync all
     do i=1,10**j
        lock (lck[1])
        count[1] = count[1] + 1
        unlock (lck[1])
     end do
     sync all
     if (this_image() == 1) then
        !print *,"Expected: ",10**j*num_images(), "Found: ", count[1]
        if (count[1] .ne. 10**j*num_images()) stop 1
     end if
     sync all
   end do
end program

