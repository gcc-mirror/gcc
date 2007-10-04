! { dg-do run }
  character (kind=kind("a")) :: u
  if (kind(u) /= kind("a")) call abort
  end
