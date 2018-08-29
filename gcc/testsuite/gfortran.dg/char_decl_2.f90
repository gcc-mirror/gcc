! { dg-do run }
  character (kind=kind("a")) :: u
  if (kind(u) /= kind("a")) STOP 1
  end
