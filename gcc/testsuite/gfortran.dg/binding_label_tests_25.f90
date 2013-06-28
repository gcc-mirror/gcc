! { dg-do compile }
!
! PR fortran/48858
! PR fortran/55465
!
! Seems to be regarded as valid, even if it is doubtful
!


module m_odbc_if
  implicit none

  interface sql_set_env_attr
    function sql_set_env_attr_int( input_handle,attribute,value,length ) &
                                   result(res) bind(C,name="SQLSetEnvAttr")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), value :: input_handle
      integer(c_int), value :: attribute
      integer(c_int), value :: value  ! <<<< HERE: int passed by value (int with ptr address)
      integer(c_int), value :: length      
      integer(c_short) :: res
    end function
    function sql_set_env_attr_ptr( input_handle,attribute,value,length ) &
                                   result(res) bind(C,name="SQLSetEnvAttr")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), value :: input_handle
      integer(c_int), value :: attribute
      type(c_ptr), value :: value ! <<< HERE: "void *" (pointer address)
      integer(c_int), value :: length      
      integer(c_short) :: res
    end function
  end interface
end module

module graph_partitions
  use,intrinsic :: iso_c_binding

  interface Cfun
     subroutine cfunc1 (num, array) bind(c, name="Cfun")
       import :: c_int
       integer(c_int),value :: num
       integer(c_int)       :: array(*) ! <<< HERE: int[]
     end subroutine cfunc1

     subroutine cfunf2 (num, array) bind(c, name="Cfun")
       import :: c_int, c_ptr
       integer(c_int),value :: num
       type(c_ptr),value    :: array ! <<< HERE: void*
     end subroutine cfunf2
  end interface
end module graph_partitions

program test
  use graph_partitions
  integer(c_int) :: a(100)

  call Cfun (1, a)
  call Cfun (2, C_NULL_PTR)
end program test
