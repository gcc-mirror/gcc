! { dg-do compile }
module strings

   type string
      integer :: len = 0, size = 0
      character, pointer :: chars(:) => null()
   end type string

   interface length
      module procedure len_s
   end interface

   interface char
      module procedure s_to_c, s_to_slc  
   end interface

   interface uppercase
      module procedure uppercase_c
   end interface

   interface replace
      module procedure replace_ccs
   end interface

   contains

      elemental function len_s(s)
         type(string), intent(in) :: s
         integer :: len_s
      end function len_s

      pure function s_to_c(s)
         type(string),intent(in) :: s
         character(length(s)) :: s_to_c
      end function s_to_c

      pure function s_to_slc(s,long)
         type(string),intent(in) :: s
         integer, intent(in) :: long
         character(long) :: s_to_slc
      end function s_to_slc

      pure function lr_sc_s(s,start,ss) result(l)
         type(string), intent(in) :: s
         character(*), intent(in) :: ss
         integer, intent(in)  :: start
         integer :: l
      end function lr_sc_s

      pure function lr_ccc(s,tgt,ss,action) result(l)
         character(*), intent(in) :: s,tgt,ss,action
         integer :: l
         select case(uppercase(action))
         case default
         end select
      end function lr_ccc

      function replace_ccs(s,tgt,ss) result(r)
         character(*), intent(in)             :: s,tgt
         type(string), intent(in)             :: ss
         character(lr_ccc(s,tgt,char(ss),'first'))  :: r
      end function replace_ccs

      pure function uppercase_c(c)
         character(*), intent(in) :: c
         character(len(c)) :: uppercase_c
      end function uppercase_c

end module strings
