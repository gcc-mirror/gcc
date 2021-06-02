// { dg-prune-output "warning: unused name" } as there are many of these expected.

fn main() {
    let hex: i32 = 0xFF;
    let binary: i32 = 0b11110000;
    let oct: i32 = 0o70;

    let hex_u8: u8 = 0xFF_u8;
    let bin_u16: u16 = 0b1111000011110000_u16;
    let oct: u32 = 0o70_u32;
}
