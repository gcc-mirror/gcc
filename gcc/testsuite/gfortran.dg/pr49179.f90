! { dg-options " -O -findirect-inlining" }
function more_OK (fcn)
  character(*) more_OK
  character (*), external :: fcn
  more_OK = fcn ()
end function more_OK
  character(4) :: answer
  character(4), external :: is_OK, more_OK
  answer = more_OK (is_OK)
contains
END
