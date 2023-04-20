macro_rules! multi {
    ($( $a:ident )? $( + $b:ident )?) => {
        {
            $( let $a: u32 )?;
        }
    }
}

pub fn foo() {
    multi!(_a);
}
