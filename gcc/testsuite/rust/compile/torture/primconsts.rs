const TRUE: bool = true;
const FALSE: bool = !TRUE;

const U8ZERO: u8 = 0;
const U8ONE: u8 = U8ZERO + 1;
const U16ZERO: u16 = 0;
const U16ONE: u16 = U16ZERO + 1;
const U32ZERO: u32 = 0;
const U32ONE: u32 = U32ZERO + 1;
const U64ZERO: u64 = 0;
const U64ONE: u64 = U64ZERO + 1;
const U128ZERO: u128 = 0;
const U128ONE: u128 = U128ZERO + 1;

const I8ZERO: i8 = 0;
const I8ONE: i8 = I8ZERO + 1;
const I16ZERO: i16 = 0;
const I16ONE: i16 = I16ZERO + 1;
const I32ZERO: i32 = 0;
const I32ONE: i32 = I32ZERO + 1;
const I64ZERO: i64 = 0;
const I64ONE: i64 = I64ZERO + 1;
const I128ZERO: i128 = 0;
const I128ONE: i128 = I128ZERO + 1;

const F32ZERO: f32 = 0.0;
const F32ONE: f32 = F32ZERO + 1.0;
const F64ZERO: f64 = 0.0;
const F64ONE: f64 = F64ZERO + 1.0;

const USIZEZERO: usize = 0;
const USIZEONE: usize = USIZEZERO + 1;
const ISIZEZERO: isize = 0;
const ISIZEONE: isize = ISIZEZERO + 1;

/* Not yet supported 
const CHARPI: char = '\u{03C0}';
const STRHELLO: &str = "Hello World!";
*/

extern "C" { fn abort (); }

pub fn main ()
{
  if TRUE == FALSE { unsafe { abort (); } }
  if U8ZERO > U8ONE { unsafe { abort (); } }
  if U16ZERO > U16ONE { unsafe { abort (); } }
  if U32ZERO > U32ONE { unsafe { abort (); } }
  if U64ZERO > U64ONE { unsafe { abort (); } }
  if U128ZERO > U128ONE { unsafe { abort (); } }

  if I8ONE <= I8ZERO { unsafe { abort (); } }
  if I16ONE <= I16ZERO { unsafe { abort (); } }
  if I32ONE <= I32ZERO { unsafe { abort (); } }
  if I64ONE <= I64ZERO { unsafe { abort (); } }
  if I128ONE <= I128ZERO { unsafe { abort (); } }

  if F32ZERO + F32ONE != F32ONE { unsafe { abort (); } }
  if F64ZERO + F64ONE != F64ONE { unsafe { abort (); } }

  if USIZEZERO + USIZEONE - USIZEONE + USIZEZERO != USIZEZERO
    {
      unsafe { abort (); }
    }
  if ISIZEZERO + ISIZEONE - ISIZEONE + ISIZEZERO != ISIZEZERO
    {
      unsafe { abort (); }
    }

 // if CHARPI != '\u{03c0}'  { unsafe { abort (); } }
 // if STRHELLO != "Hello World!" { unsafe { abort (); } }
}
