! { dg-do compile }
program p
 type t
  character(1) :: c(1)=[1] ! { dg-error "convert INTEGER.4. to CHARACTER.1." }
 end type
end
