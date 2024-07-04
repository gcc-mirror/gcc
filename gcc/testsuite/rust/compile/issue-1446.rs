pub fn to_le(this: u32) -> u32 {
    #[cfg(target_endian = "little")]
    {
        this
    }
    #[cfg(not(target_endian = "little"))]
    {
        this.swap_bytes()
    }
}
