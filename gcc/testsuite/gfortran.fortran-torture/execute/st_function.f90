! Program to test STATEMENT function
program st_fuction
   call simple_case 
   call with_function_call
   call with_character_dummy
   call with_derived_type_dummy
   call with_pointer_dummy
   call multiple_eval

contains
   subroutine simple_case
      integer st1, st2
      integer c(10, 10)
      st1 (i, j) = i + j
      st2 (i, j) = c(i, j)

      if (st1 (1, 2) .ne. 3) call abort
      c = 3
      if (st2 (1, 2) .ne. 3 .or. st2 (2, 3) .ne. 3) call abort
   end subroutine

   subroutine with_function_call
      integer fun, st3
      st3 (i, j) = fun (i) + fun (j)

      if (st3 (fun (2), 4) .ne. 16) call abort
   end subroutine

   subroutine with_character_dummy
      character (len=4) s1, s2, st4
      character (len=10) st5, s0
      st4 (i, j) = "0123456789"(i:j)
      st5 (s1, s2) = s1 // s2

      if (st4 (1, 4) .ne. "0123" ) call abort
      if (st5 ("01", "02") .ne. "01  02    ") call abort
   end subroutine

   subroutine with_derived_type_dummy
      type person
         integer age
         character (len=50) name
      end type person
      type (person) me, p, tom
      type (person) st6
      st6 (p) = p

      me%age = 5
      me%name = "Tom"
      tom = st6 (me)
      if (tom%age .ne. 5) call abort
      if (tom%name .gt. "Tom") call abort
   end subroutine

   subroutine with_pointer_dummy
      character(len=4), pointer:: p, p1
      character(len=4), target:: i
      character(len=6) a
      a (p) = p // '10'

      p1 => i
      i = '1234'
      if (a (p1) .ne. '123410') call abort
   end subroutine

   subroutine multiple_eval
      integer st7, fun2, fun

      st7(i) = i + fun(i)

      if (st7(fun2(10)) .ne. 3) call abort
   end subroutine
end

! This functon returns the argument passed on the previous call.
integer function fun2 (i)
  integer i
  integer, save :: val = 1

  fun2 = val
  val = i
end function

integer function fun (i)
   integer i
   fun = i * 2
end function
