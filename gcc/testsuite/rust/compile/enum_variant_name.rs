// { dg-additional-options "-w -frust-name-resolution-2.0" }
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
