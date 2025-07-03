// { dg-additional-options "-w" }
struct E1;

enum Test {
    E1 = {
        let x = E1;
        {
            let x = E1;
        }
        0
    },
}
