! { dg-do run }
! { dg-options "-fbackslash" }

  call testme(test("foo"), test4(4_"foo"), 1)
  call testme(test(""), test4(4_""), 1)
  call testme(test("gee"), test4(4_"gee"), 4)
  call testme(test("bar"), test4(4_"bar"), 1)
  call testme(test("magi"), test4(4_"magi"), 4)
  call testme(test("magic"), test4(4_"magic"), 2)
  call testme(test("magic   "), test4(4_"magic   "), 2)
  call testme(test("magica"), test4(4_"magica"), 4)
  call testme(test("freeze"), test4(4_"freeze"), 3)
  call testme(test("freeze "), test4(4_"freeze "), 3)
  call testme(test("frugal"), test4(4_"frugal"), 3)
  call testme(test("frugal "), test4(4_"frugal "), 3)
  call testme(test("frugal \x01"), test4(4_"frugal \x01"), 3)
  call testme(test("frugal \xFF"), test4(4_"frugal \xFF"), 4)

contains
  integer function test(s)
    character(len=*) :: s
  
    select case (s)
      case ("":"foo")
        test = 1
      case ("magic")
        test = 2
      case ("freeze":"frugal")
        test = 3
      case default
        test = 4
    end select
  end function test

  integer function test4(s)
    character(kind=4,len=*) :: s
  
    select case (s)
      case (4_"":4_"foo")
        test4 = 1
      case (4_"magic")
        test4 = 2
      case (4_"freeze":4_"frugal")
        test4 = 3
      case default
        test4 = 4
    end select
  end function test4

  subroutine testme(x,y,z)
    integer :: x, y, z
    if (x /= y) call abort
    if (x /= z) call abort
  end subroutine testme
end
