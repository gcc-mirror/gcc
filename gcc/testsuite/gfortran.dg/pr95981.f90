! { dg-do compile }

program p
   type t
   end type
   class(t) :: x(:)        ! { dg-error "must be dummy, allocatable or pointer" }
   type(t) :: y(size(x,1)) ! { dg-error "must be constant of INTEGER type" }
end

