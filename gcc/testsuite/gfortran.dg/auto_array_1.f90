! { dg-do run }
! PR fortran/17077.
! Automatic arrays are allocated on the heap.  When used as an actual argument
! we were passing the address of the pointer, not the pointer itself.

program p
   implicit none
   integer:: n,m

   n = 3
   call foo(n)
contains

   subroutine foo(m)
      integer:: m,i
      integer:: z(m,m)
    
      z = 0

      call foo1(m,z)
    
      ! Check it worked.
      if (any (z .ne. reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/)))) &
        call abort
    end subroutine foo

    subroutine foo1(n,x)
       integer:: n,i,j
       integer:: x(n,n)
    
       ! Assign values to x.
       do i=1,n
          do j=1,n
             x(j,i)=j+(i-1)*n
          enddo
       enddo
    end subroutine foo1
end program    
