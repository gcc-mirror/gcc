/* { dg-do run } */
/* { dg-options "-O2" } */
volatile int v;

__attribute__((noipa)) int
f (__INT32_TYPE__ a)
{
  switch (a)
    {
    case 1: case 3: case 5: case 7: case 9: case 11: case 13: case 15: case 17: case 19: case 21: case 23: case 25: case 27: case 29:
    case 31: case 33: case 35: case 37: case 39: case 41: case 43: case 45: case 47: case 49: case 51: case 53: case 55: case 57: case 59:
    case 61: case 63: case 65: case 67: case 69: case 71: case 73: case 75: case 77: case 79: case 81: case 83: case 85: case 87: case 89:
    case 91: case 93: case 95: case 97: case 99: case 101: case 103: case 105: case 107: case 109: case 111: case 113: case 115: case 117: case 119:
    case 121: case 123: case 125: case 127: case 129: case 131: case 133: case 135: case 137: case 139: case 141: case 143: case 145: case 147: case 149:
    case 151: case 153: case 155: case 157: case 159: case 161: case 163: case 165: case 167: case 169: case 171: case 173: case 175: case 177: case 179:
    case 181: case 183: case 185: case 187: case 189: case 191: case 193: case 195: case 197: case 199: case 201: case 203: case 205: case 207: case 209:
    case 211: case 213: case 215: case 217: case 219: case 221: case 223: case 225: case 227: case 229: case 231: case 233: case 235: case 237: case 239:
    case 241: case 243: case 245: case 247: case 249: case 251: case 253: case 255: case 257: case 259: case 261: case 263: case 265: case 267: case 269:
    case 271: case 273: case 275: case 277: case 279: case 281: case 283: case 285: case 287: case 289: case 291: case 293: case 295: case 297: case 299:
    case 301: case 303: case 305: case 307: case 309: case 311: case 313: case 315: case 317: case 319: case 321: case 323: case 325: case 327: case 329:
    case 331: case 333: case 335: case 337: case 339: case 341: case 343: case 345: case 347: case 349: case 351: case 353: case 355: case 357: case 359:
    case 361: case 363: case 365: case 367: case 369: case 371: case 373: case 375: case 377: case 379: case 381: case 383: case 385: case 387: case 389:
    case 391: case 393: case 395: case 397: case 399: case 401: case 403: case 405: case 407: case 409: case 411: case 413: case 415: case 417: case 419:
    case 421: case 423: case 425: case 427: case 429: case 431: case 433: case 435: case 437: case 439: case 441: case 443: case 445: case 447: case 449:
    case 451: case 453: case 455: case 457: case 459: case 461: case 463: case 465: case 467: case 469: case 471: case 473: case 475: case 477: case 479:
    case 481: case 483: case 485: case 487: case 489: case 491: case 493: case 495: case 497: case 499: case 501: case 503: case 505: case 507: case 509:
      break;
    default:
      return 0;
    }
  v += 1; v += 2; v += 3; v += 4; v += 5; v += 6; v += 7; v += 8; v += 9; v += 10;
  v += 11; v += 12; v += 13; v += 14; v += 15; v += 16; v += 17; v += 18; v += 19; v += 20;
  v += 21; v += 22; v += 23; v += 24; v += 25; v += 26; v += 27; v += 28; v += 29; v += 30;
  v += 31; v += 32; v += 33; v += 34; v += 35; v += 36; v += 37; v += 38; v += 39; v += 40;
  switch (a)
    {
    case 0 ... 100000:
      return 1;
    case 200000:
      return 3;
    default:
      return 2;
    }
}

int
main (void)
{
  if (f (3) != 1)
    __builtin_abort ();
  if (f (2) != 0)
    __builtin_abort ();
  return 0;
}

