package main

func fn(s string) string {
  if len(s) != 3 {
    panic(0)
  }
  i := len(s) - 1;
  return s + s[0 : i];
}

func main() {
  s := fn("abc");
  if s != "abcab" {
    panic(1)
  }
}
