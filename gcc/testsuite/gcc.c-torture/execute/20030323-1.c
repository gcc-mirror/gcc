/* PR opt/10116 */
/* { dg-require-effective-target return_address } */
/* Removed tablejump while label still in use; this is really a link test.  */

void *NSReturnAddress(int offset)
{
	switch (offset) {
	case 0:  return __builtin_return_address(0 + 1);
	case 1:  return __builtin_return_address(1 + 1);
	case 2:  return __builtin_return_address(2 + 1);
	case 3:  return __builtin_return_address(3 + 1);
	case 4:  return __builtin_return_address(4 + 1);
	case 5:  return __builtin_return_address(5 + 1);
	case 6:  return __builtin_return_address(6 + 1);
	case 7:  return __builtin_return_address(7 + 1);
	case 8:  return __builtin_return_address(8 + 1);
	case 9:  return __builtin_return_address(9 + 1);
	case 10: return __builtin_return_address(10 + 1);
	case 11: return __builtin_return_address(11 + 1);
	case 12: return __builtin_return_address(12 + 1);
	case 13: return __builtin_return_address(13 + 1);
	case 14: return __builtin_return_address(14 + 1);
	case 15: return __builtin_return_address(15 + 1);
	case 16: return __builtin_return_address(16 + 1);
	case 17: return __builtin_return_address(17 + 1);
	case 18: return __builtin_return_address(18 + 1);
	case 19: return __builtin_return_address(19 + 1);
	case 20: return __builtin_return_address(20 + 1);
	case 21: return __builtin_return_address(21 + 1);
	case 22: return __builtin_return_address(22 + 1);
	case 23: return __builtin_return_address(23 + 1);
	case 24: return __builtin_return_address(24 + 1);
	case 25: return __builtin_return_address(25 + 1);
	case 26: return __builtin_return_address(26 + 1);
	case 27: return __builtin_return_address(27 + 1);
	case 28: return __builtin_return_address(28 + 1);
	case 29: return __builtin_return_address(29 + 1);
	case 30: return __builtin_return_address(30 + 1);
	case 31: return __builtin_return_address(31 + 1);
	case 32: return __builtin_return_address(32 + 1);
	case 33: return __builtin_return_address(33 + 1);
	case 34: return __builtin_return_address(34 + 1);
	case 35: return __builtin_return_address(35 + 1);
	case 36: return __builtin_return_address(36 + 1);
	case 37: return __builtin_return_address(37 + 1);
	case 38: return __builtin_return_address(38 + 1);
	case 39: return __builtin_return_address(39 + 1);
	case 40: return __builtin_return_address(40 + 1);
	case 41: return __builtin_return_address(41 + 1);
	case 42: return __builtin_return_address(42 + 1);
	case 43: return __builtin_return_address(43 + 1);
	case 44: return __builtin_return_address(44 + 1);
	case 45: return __builtin_return_address(45 + 1);
	case 46: return __builtin_return_address(46 + 1);
	case 47: return __builtin_return_address(47 + 1);
	case 48: return __builtin_return_address(48 + 1);
	case 49: return __builtin_return_address(49 + 1);
	case 50: return __builtin_return_address(50 + 1);
	case 51: return __builtin_return_address(51 + 1);
	case 52: return __builtin_return_address(52 + 1);
	case 53: return __builtin_return_address(53 + 1);
	case 54: return __builtin_return_address(54 + 1);
	case 55: return __builtin_return_address(55 + 1);
	case 56: return __builtin_return_address(56 + 1);
	case 57: return __builtin_return_address(57 + 1);
	case 58: return __builtin_return_address(58 + 1);
	case 59: return __builtin_return_address(59 + 1);
	case 60: return __builtin_return_address(60 + 1);
	case 61: return __builtin_return_address(61 + 1);
	case 62: return __builtin_return_address(62 + 1);
	case 63: return __builtin_return_address(63 + 1);
	case 64: return __builtin_return_address(64 + 1);
	case 65: return __builtin_return_address(65 + 1);
	case 66: return __builtin_return_address(66 + 1);
	case 67: return __builtin_return_address(67 + 1);
	case 68: return __builtin_return_address(68 + 1);
	case 69: return __builtin_return_address(69 + 1);
	case 70: return __builtin_return_address(70 + 1);
	case 71: return __builtin_return_address(71 + 1);
	case 72: return __builtin_return_address(72 + 1);
	case 73: return __builtin_return_address(73 + 1);
	case 74: return __builtin_return_address(74 + 1);
	case 75: return __builtin_return_address(75 + 1);
	case 76: return __builtin_return_address(76 + 1);
	case 77: return __builtin_return_address(77 + 1);
	case 78: return __builtin_return_address(78 + 1);
	case 79: return __builtin_return_address(79 + 1);
	case 80: return __builtin_return_address(80 + 1);
	case 81: return __builtin_return_address(81 + 1);
	case 82: return __builtin_return_address(82 + 1);
	case 83: return __builtin_return_address(83 + 1);
	case 84: return __builtin_return_address(84 + 1);
	case 85: return __builtin_return_address(85 + 1);
	case 86: return __builtin_return_address(86 + 1);
	case 87: return __builtin_return_address(87 + 1);
	case 88: return __builtin_return_address(88 + 1);
	case 89: return __builtin_return_address(89 + 1);
	case 90: return __builtin_return_address(90 + 1);
	case 91: return __builtin_return_address(91 + 1);
	case 92: return __builtin_return_address(92 + 1);
	case 93: return __builtin_return_address(93 + 1);
	case 94: return __builtin_return_address(94 + 1);
	case 95: return __builtin_return_address(95 + 1);
	case 96: return __builtin_return_address(96 + 1);
	case 97: return __builtin_return_address(97 + 1);
	case 98: return __builtin_return_address(98 + 1);
	case 99: return __builtin_return_address(99 + 1);
	}
	return 0;
}

int main()
{
  return 0;
}
