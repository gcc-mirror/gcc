! { dg-do compile }
program main
use iso_c_binding
  interface
     subroutine p1(f, a1, a2, a3, a4) bind(c, name='printf') ! Doubtful use ...
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: f
       integer(c_int), value :: a1, a3
       real(c_double), value :: a2, a4
     end subroutine p1

     subroutine p2(f, a1, a2, a3, a4) bind(c, name='printf') ! ... with incompatible interfaces
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: f
       real(c_double), value :: a1, a3
       integer(c_int), value :: a2, a4
     end subroutine p2
  end interface

  type(c_ptr) :: f_ptr
  character(len=20), target :: format

  f_ptr = c_loc(format(1:1))

  format = 'Hello %d %f %d %f\n' // char(0)
  call p1(f_ptr, 10, 1.23d0, 20, 2.46d0)

  format = 'World %f %d %f %d\n' // char(0)
  call p2(f_ptr, 1.23d0, 10, 2.46d0, 20)
end program main
