package main

func main() {
  v := make(map[int] int);
  v[0] = 0;
  v[1000000] = 1;
  if v[0] != 0 {
    panic(1)
  }
  val, present := v[0];
  if !present || val != 0 {
    panic(2)
  }
  val = 5;
  val, present = v[1];
  if present || val != 0 {
    panic(3);
  }
  if v[2] != 0 {
    panic(4)
  }
  val, present = v[2];
  if present {
    panic(5)
  }
  if len(v) != 2 {
    panic(6)
  }
  delete(v, 0)
  if len(v) != 1 {
    panic(7)
  }

  w := make(map[string] string);
  if len(w) != 0 {
    panic(8)
  }
  w["Hello"] = "world";
  w["Goodbye"] = "sweet prince";
  if w["Hello"] != "world" {
    panic(9)
  }
  if w["Hej"] != "" {
    panic(10)
  }
}
