package main

func main() {
  type s struct { x int; y int; };
  var ret s = s{1, 2};
  if ret.y - (ret.x + ret.x) != 0 { panic(0) }
}
