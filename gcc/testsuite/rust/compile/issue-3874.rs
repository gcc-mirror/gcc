#![feature(no_core)]
#![no_core]

fn wow(){
    &#[serde]
} // { dg-error "found unexpected token .\}. in null denotation" "" { target *-*-* } . }
