fn foo(pred: bool) -> u8 {
    if pred { // { dg-error "mismatched types" }
        1
    }
    3
}

fn main(){
}
