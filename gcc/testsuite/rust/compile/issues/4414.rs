#![feature(no_core)]
#![no_core]

trait Speak : Sized {
    fn say(&self, s:&str) -> String;
    fn hi(&self) -> String { hello(self) }
}

fn hello<S:Speak>(s:&S) -> String{
    s.say("hello")
}

impl Speak for assert_eq!(Some(3).hi(), "something!hello: 3".to_string()) { 
    fn say(&self, s:&str) -> String {
        format!("{}: {}", s, *self)
    }
}

impl<T: Speak> Speak for Option<T> {
    fn say(&self, s:&str) -> String {
        match something!hello - none { // { dg-error "unexpected token 'identifier'|failed to parse scrutinee" }
            None => format!("{} - none", s),
            Some(ref x) => { format!("something!{}", x.say(s)) }
        }
    } 
} // { dg-error "could not parse definition" }

fn hello<S:Speak>(s:&S) -> String{ // { dg-error "failed to parse trait impl item" }
    s.say("hello")
}