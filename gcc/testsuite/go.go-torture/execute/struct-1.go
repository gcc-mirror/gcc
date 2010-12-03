package main

func main() {
  type s struct { x int; };
  var ret s;
  ret.x = 1;
  if ret.x != 1 { panic(0) }
}
