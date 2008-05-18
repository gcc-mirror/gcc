! { dg-do compile }

  character(kind=1,len=20) :: s1
  character(kind=4,len=20) :: s4

  select case (s1)
    case ("":4_"foo") ! { dg-error "must be of kind" }
      test = 1
    case (4_"gee") ! { dg-error "must be of kind" }
      test = 1
    case ("bar")
      test = 1
    case default
      test = 4
  end select

  select case (s4)
    case ("":4_"foo") ! { dg-error "must be of kind" }
      test = 1
    case (4_"gee")
      test = 1
    case ("bar") ! { dg-error "must be of kind" }
      test = 1
    case default
      test = 4
  end select

  select case (s4)
    case (4_"foo":4_"bar")
      test = 1
    case (4_"foo":4_"gee") ! { dg-error "overlaps with CASE label" }
      test = 1
    case (4_"foo") ! { dg-error "overlaps with CASE label" }
      test = 1
  end select

end
