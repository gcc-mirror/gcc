! { dg-do compile }
module op

   implicit none

   type a
      integer i
   end type a

   type b
      real i
   end type b

   interface operator(==)
      module procedure f1
   end interface operator(.eq.)
   interface operator(.eq.)
      module procedure f2
   end interface operator(==)

   interface operator(/=)
      module procedure f1
   end interface operator(.ne.)
   interface operator(.ne.)
      module procedure f2
   end interface operator(/=)

   interface operator(<=)
      module procedure f1
   end interface operator(.le.)
   interface operator(.le.)
      module procedure f2
   end interface operator(<=)

   interface operator(<)
      module procedure f1
   end interface operator(.lt.)
   interface operator(.lt.)
      module procedure f2
   end interface operator(<)

   interface operator(>=)
      module procedure f1
   end interface operator(.ge.)
   interface operator(.ge.)
      module procedure f2
   end interface operator(>=)

   interface operator(>)
      module procedure f1
   end interface operator(.gt.)
   interface operator(.gt.)
      module procedure f2
   end interface operator(>)

   contains

      function f2(x,y)
         logical f2
         type(a), intent(in) :: x, y
      end function f2

      function f1(x,y)
         logical f1
         type(b), intent(in) :: x, y
      end function f1

end module op
