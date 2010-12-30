package main

func main() {
  var v1 = 1;
  var v2 = 1;
  var v3 = (v1 + v2) / (v1 + v2);
  var v4 = (v3 * v3) % (v3 * v3);
  if v4 != 0 {
    panic(0)
  }
}
