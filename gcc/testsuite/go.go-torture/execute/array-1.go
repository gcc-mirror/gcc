package main
var a [2]int;
func fn() {
  a[0] = 1;
  a[1] = 1;
}
func main() {
  fn();
  if a[0] != a[1] { panic(0) }
}
