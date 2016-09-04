! { dg-do compile }
program picky
character(len=:), parameter :: a="whoops" ! { dg-error "POINTER or ALLOCATABLE" }
character(len=:) :: b="whoops" ! { dg-error "POINTER or ALLOCATABLE" }
character(len=:) :: good
pointer good
end program picky
