fn main() {
    let x = 1;
    {
        let x = true;
        {
            x = false;
        }
    }
    let x = x + 1;
}
