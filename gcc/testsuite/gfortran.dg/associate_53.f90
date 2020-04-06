! { dg-do compile }
!
! PR fortran/93363
!
! Contributed by G. Steinmetz

program p
   type t
      integer :: a
   end type
   type(t) :: z
   z = t(1)
   associate (var1 => t)  ! { dg-error "Derived type 't' cannot be used as a variable" }
   end associate
end

subroutine sub
   if (f() /= 1) stop
   associate (var2 => f)  ! { dg-error "Associating entity 'f' at .1. is a procedure name" }
   end associate
   block
      block
        associate (var2a => f)  ! { dg-error "Associating entity 'f' at .1. is a procedure name" }
        end associate
      end block
    end block
contains
   integer function f()
      f = 1
      associate (var3 => f)
      end associate
      block
        block
          associate (var4 => f)
          end associate
        end block
      end block
   end
   integer recursive function f2() result(res)
      res = 1
      associate (var5 => f2)  ! { dg-error "Associating entity 'f2' at .1. is a procedure name" }
      end associate
      block
        block
          associate (var6 => f2)  ! { dg-error "Associating entity 'f2' at .1. is a procedure name" }
          end associate
        end block
      end block
   end
   subroutine subsub
      associate (var7 => f)  ! { dg-error "Associating entity 'f' at .1. is a procedure name" }
      end associate
      block
        block
          associate (var8 => f)  ! { dg-error "Associating entity 'f' at .1. is a procedure name" }
          end associate
        end block
      end block
   end
end

subroutine sub2
   interface g
      procedure s
   end interface
   associate (var9 => g)  ! { dg-error "Associating entity 'g' at .1. is a procedure name" }
   end associate
contains
   subroutine s
   end
end
