// { dg-options "-w" } Currently there are a lot of warnings produced from inside clone/copy
// builtins

#[lang = "copy"]
trait Copy {}

#[lang = "clone"]
trait Clone {
    fn clone(&self) -> Self;
}

#[derive(Copy)]
#[derive(Clone)]
struct Empty;

#[derive(Copy,Clone)]
struct Empty2;
