! { dg-do compile }
program p
   type t
      character(3) :: c(1) = 'a' // ['b']
   end type
end
