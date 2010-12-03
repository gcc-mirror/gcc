package main

func fn(s string) int {
  if s[0] != 'a' || s[1] != 'b' || s[2] != 'c' {
    panic(0);
  }
  return len(s);
}

func main() {
  s := "abc";
  if fn(s) != 3 {
    panic(1);
  }
}
