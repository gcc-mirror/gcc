// fake function
pub fn swap_bytes(this: u32) -> u32 {
    (((this) & 0xff000000) >> 24)
        | (((this) & 0x00ff0000) >> 8)
        | (((this) & 0x0000ff00) << 8)
        | (((this) & 0x000000ff) << 24)
}

pub fn to_le(this: u32) -> u32 {
    #[cfg(target_endian = "little")]
    {
        this
    }
    #[cfg(not(target_endian = "little"))]
    {
        swap_bytes(this)
    }
}
