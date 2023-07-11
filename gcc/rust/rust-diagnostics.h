// Copyright (C) 2020-2023 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// rust-diagnostics.h -- interface to diagnostic reporting   -*- C++ -*-

#ifndef RUST_DIAGNOSTICS_H
#define RUST_DIAGNOSTICS_H

#include "rust-linemap.h"

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)
#define RUST_ATTRIBUTE_GCC_DIAG(m, n)                                          \
  __attribute__ ((__format__ (__gcc_tdiag__, m, n)))                           \
    __attribute__ ((__nonnull__ (m)))
#else
#define RUST_ATTRIBUTE_GCC_DIAG(m, n)
#endif

// These declarations define the interface through which the frontend
// reports errors and warnings. These functions accept printf-like
// format specifiers (e.g. %d, %f, %s, etc), with the following additional
// extensions:
//
//  1.  'q' qualifier may be applied to a specifier to add quoting, e.g.
//      %qd produces a quoted decimal output, %qs a quoted string output.
//      [This extension is supported only with single-character format
//      specifiers].
//
//  2.  %m specifier outputs value of "strerror(errno)" at time of call.
//
//  3.  %< outputs an opening quote, %> a closing quote.
//
// All other format specifiers are as defined by 'sprintf'. The final resulting
// message is then sent to the back end via rust_be_error_at/rust_be_warning_at.

// clang-format off
// simple location

// https://gist.github.com/MahadMuhammad/8c9d5fc88ea18d8c520937a8071d4185
enum class ErrorCode 
{ 
  E0001, // this error code is no longer emitted by the compiler
  E0002, // this error code is no longer emitted by the compiler
  E0004,
  E0005,
  E0007, // this error code is no longer emitted by the compiler
  E0009, // this error code is no longer emitted by the compiler
  E0010,
  E0013,
  E0014, // this error code is no longer emitted by the compiler
  E0015,
  E0023,
  E0025,
  E0026,
  E0027,
  E0029,
  E0030,
  E0033,
  E0034,
  E0038,
  E0040,
  E0044,
  E0045,
  E0046,
  E0049,
  E0050,
  E0053,
  E0054,
  E0055,
  E0057,
  E0059,
  E0060,
  E0061,
  E0062,
  E0063,
  E0067,
  E0069,
  E0070,
  E0071,
  E0072,
  E0073, // this error code is no longer emitted by the compiler
  E0074, // this error code is no longer emitted by the compiler
  E0075,
  E0076,
  E0077,
  E0080,
  E0081,
  E0084,
  E0087, // this error code is no longer emitted by the compiler
  E0088, // this error code is no longer emitted by the compiler
  E0089, // this error code is no longer emitted by the compiler
  E0090, // this error code is no longer emitted by the compiler
  E0091,
  E0092,
  E0093,
  E0094,
  E0106,
  E0107,
  E0109,
  E0110, // this error code is no longer emitted by the compiler
  E0116,
  E0117,
  E0118,
  E0119,
  E0120,
  E0121,
  E0124,
  E0128,
  E0130,
  E0131,
  E0132,
  E0133,
  E0136, // this error code is no longer emitted by the compiler
  E0137, // this error code is no longer emitted by the compiler
  E0138,
  E0139, // this error code is no longer emitted by the compiler
  E0152,
  E0154, // this error code is no longer emitted by the compiler
  E0158,
  E0161,
  E0162, // this error code is no longer emitted by the compiler
  E0164,
  E0165, // this error code is no longer emitted by the compiler
  E0170,
  E0178,
  E0183,
  E0184,
  E0185,
  E0186,
  E0191,
  E0192, // this error code is no longer emitted by the compiler
  E0193, // this error code is no longer emitted by the compiler
  E0195,
  E0197,
  E0198,
  E0199,
  E0200,
  E0201,
  E0203,
  E0204,
  E0205, // this error code is no longer emitted by the compiler
  E0206,
  E0207,
  E0208, // this error code is no longer emitted by the compiler
  E0210,
  E0211, // this error code is no longer emitted by the compiler
  E0212,
  E0214,
  E0220,
  E0221,
  E0222,
  E0223,
  E0224,
  E0225,
  E0226,
  E0227,
  E0228,
  E0229,
  E0230,
  E0231,
  E0232,
  E0243, // this error code is no longer emitted by the compiler
  E0244, // this error code is no longer emitted by the compiler
  E0251, // this error code is no longer emitted by the compiler
  E0252,
  E0253,
  E0254,
  E0255,
  E0256, // this error code is no longer emitted by the compiler
  E0259,
  E0260,
  E0261,
  E0262,
  E0263, // this error code is no longer emitted by the compiler
  E0264,
  E0267,
  E0268,
  E0271,
  E0275,
  E0276,
  E0277,
  E0281, // this error code is no longer emitted by the compiler
  E0282,
  E0283,
  E0284,
  E0297, // this error code is no longer emitted by the compiler
  E0301, // this error code is no longer emitted by the compiler
  E0302, // this error code is no longer emitted by the compiler
  E0303, // this error code is no longer emitted by the compiler
  E0307,
  E0308,
  E0309,
  E0310,
  E0311,
  E0312, // this error code is no longer emitted by the compiler
  E0316,
  E0317,
  E0320,
  E0321,
  E0322,
  E0323,
  E0324,
  E0325,
  E0326,
  E0328,
  E0329, // this error code is no longer emitted by the compiler
  E0364,
  E0365,
  E0366,
  E0367,
  E0368,
  E0369,
  E0370,
  E0371,
  E0373,
  E0374,
  E0375,
  E0376,
  E0377,
  E0378,
  E0379,
  E0380,
  E0381,
  E0382,
  E0383, // this error code is no longer emitted by the compiler
  E0384,
  E0386, // this error code is no longer emitted by the compiler
  E0387, // this error code is no longer emitted by the compiler
  E0388, // this error code is no longer emitted by the compiler
  E0389, // this error code is no longer emitted by the compiler
  E0390,
  E0391,
  E0392,
  E0393,
  E0398, // this error code is no longer emitted by the compiler
  E0399,
  E0401,
  E0403,
  E0404,
  E0405,
  E0407,
  E0408,
  E0409,
  E0411,
  E0412,
  E0415,
  E0416,
  E0422,
  E0423,
  E0424,
  E0425,
  E0426,
  E0428,
  E0429,
  E0430,
  E0431,
  E0432,
  E0433,
  E0434,
  E0435,
  E0436,
  E0437,
  E0438,
  E0439, // this error code is no longer emitted by the compiler
  E0445,
  E0446,
  E0447, // this error code is no longer emitted by the compiler
  E0448, // this error code is no longer emitted by the compiler
  E0449,
  E0451,
  E0452,
  E0453,
  E0454,
  E0455,
  E0457,
  E0458,
  E0459,
  E0460,
  E0461,
  E0462,
  E0463,
  E0464,
  E0466,
  E0468,
  E0469,
  E0472,
  E0476,
  E0477, // this error code is no longer emitted by the compiler
  E0478,
  E0482, // this error code is no longer emitted by the compiler
  E0491,
  E0492,
  E0493,
  E0495, // this error code is no longer emitted by the compiler
  E0496,
  E0497, // this error code is no longer emitted by the compiler
  E0498,
  E0499,
  E0500,
  E0501,
  E0502,
  E0503,
  E0504, // this error code is no longer emitted by the compiler
  E0505,
  E0506,
  E0507,
  E0508,
  E0509,
  E0510,
  E0511,
  E0512,
  E0514,
  E0515,
  E0516,
  E0517,
  E0518,
  E0519,
  E0520,
  E0521,
  E0522,
  E0523, // this error code is no longer emitted by the compiler
  E0524,
  E0525,
  E0527,
  E0528,
  E0529,
  E0530,
  E0531,
  E0532,
  E0533,
  E0534,
  E0535,
  E0536,
  E0537,
  E0538,
  E0539,
  E0541,
  E0542,
  E0543,
  E0544,
  E0545,
  E0546,
  E0547,
  E0549,
  E0550,
  E0551,
  E0552,
  E0554,
  E0556,
  E0557,
  E0559,
  E0560,
  E0561,
  E0562,
  E0565,
  E0566,
  E0567,
  E0568,
  E0569,
  E0570,
  E0571,
  E0572,
  E0573,
  E0574,
  E0575,
  E0576,
  E0577,
  E0578,
  E0579,
  E0580,
  E0581,
  E0582,
  E0583,
  E0584,
  E0585,
  E0586,
  E0587,
  E0588,
  E0589,
  E0590,
  E0591,
  E0592,
  E0593,
  E0594,
  E0595, // this error code is no longer emitted by the compiler
  E0596,
  E0597,
  E0599,
  E0600,
  E0601,
  E0602,
  E0603,
  E0604,
  E0605,
  E0606,
  E0607,
  E0608,
  E0609,
  E0610,
  E0614,
  E0615,
  E0616,
  E0617,
  E0618,
  E0619, // this error code is no longer emitted by the compiler
  E0620,
  E0621,
  E0622,
  E0623,
  E0624,
  E0625,
  E0626,
  E0627,
  E0628,
  E0631,
  E0632, // this error code is no longer emitted by the compiler
  E0633, // this error code is no longer emitted by the compiler
  E0634,
  E0635,
  E0636,
  E0637,
  E0638,
  E0639,
  E0640,
  E0641,
  E0642,
  E0643,
  E0644,
  E0646,
  E0647,
  E0648,
  E0657,
  E0658,
  E0659,
  E0660, // this error code is no longer emitted by the compiler
  E0661, // this error code is no longer emitted by the compiler
  E0662, // this error code is no longer emitted by the compiler
  E0663, // this error code is no longer emitted by the compiler
  E0664, // this error code is no longer emitted by the compiler
  E0665, // this error code is no longer emitted by the compiler
  E0666,
  E0667,
  E0668, // this error code is no longer emitted by the compiler
  E0669, // this error code is no longer emitted by the compiler
  E0670,
  E0671, // this error code is no longer emitted by the compiler
  E0687, // this error code is no longer emitted by the compiler
  E0688, // this error code is no longer emitted by the compiler
  E0689,
  E0690,
  E0691,
  E0692,
  E0693,
  E0695,
  E0696,
  E0697,
  E0698,
  E0699,
  E0700,
  E0701,
  E0703,
  E0704,
  E0705,
  E0706,
  E0708,
  E0710,
  E0711,
  E0712,
  E0713,
  E0714,
  E0715,
  E0716,
  E0717,
  E0718,
  E0719,
  E0720,
  E0722,
  E0724,
  E0725,
  E0726,
  E0727,
  E0728,
  E0729,
  E0730,
  E0731,
  E0732,
  E0733,
  E0734,
  E0735,
  E0736,
  E0737,
  E0739,
  E0740,
  E0741,
  E0742,
  E0743,
  E0744,
  E0745,
  E0746,
  E0747,
  E0748,
  E0749,
  E0750,
  E0751,
  E0752,
  E0753,
  E0754,
  E0755,
  E0756,
  E0757,
  E0758,
  E0759, // this error code is no longer emitted by the compiler
  E0760, // this error code is no longer emitted by the compiler
  E0761,
  E0762,
  E0763,
  E0764,
  E0765,
  E0766,
  E0767,
  E0768,
  E0769,
  E0770,
  E0771,
  E0772, // this error code is no longer emitted by the compiler
  E0773,
  E0774,
  E0775,
  E0776,
  E0777,
  E0778,
  E0779,
  E0780,
  E0781,
  E0782,
  E0783,
  E0784,
  E0785,
  E0786,
  E0787,
  E0788,
  E0789,
  E0790,
  E0791,
  E0792,
  E0793,
  E0794
};

// Custom hash function for ErrorCode, for older version of gcc
namespace std {
    template <>
    struct hash<ErrorCode> {
      size_t operator() (const ErrorCode &error) const
      {
        return hash<std::underlying_type<ErrorCode>::type> () (
          static_cast<std::underlying_type<ErrorCode>::type> (error));
      }
    };
}

// see https://github.com/Rust-GCC/gccrs/pull/2468
#define XSTR(A) STR(A)
#define STR(A) #A
#define ERROR_CODE(NUM) NUM
#define TABLE_TO_MAP(NUM) { ErrorCode::ERROR_CODE(NUM), XSTR(ERROR_CODE(NUM)) } 

static std::unordered_map<ErrorCode,const char *> error_code_strings = {
TABLE_TO_MAP(E0001),
TABLE_TO_MAP(E0002),
TABLE_TO_MAP(E0004),
TABLE_TO_MAP(E0005),
TABLE_TO_MAP(E0007),
TABLE_TO_MAP(E0009),
TABLE_TO_MAP(E0010),
TABLE_TO_MAP(E0013),
TABLE_TO_MAP(E0014),
TABLE_TO_MAP(E0015),
TABLE_TO_MAP(E0023),
TABLE_TO_MAP(E0025),
TABLE_TO_MAP(E0026),
TABLE_TO_MAP(E0027),
TABLE_TO_MAP(E0029),
TABLE_TO_MAP(E0030),
TABLE_TO_MAP(E0033),
TABLE_TO_MAP(E0034),
TABLE_TO_MAP(E0038),
TABLE_TO_MAP(E0040),
TABLE_TO_MAP(E0044),
TABLE_TO_MAP(E0045),
TABLE_TO_MAP(E0046),
TABLE_TO_MAP(E0049),
TABLE_TO_MAP(E0050),
TABLE_TO_MAP(E0053),
TABLE_TO_MAP(E0054),
TABLE_TO_MAP(E0055),
TABLE_TO_MAP(E0057),
TABLE_TO_MAP(E0059),
TABLE_TO_MAP(E0060),
TABLE_TO_MAP(E0061),
TABLE_TO_MAP(E0062),
TABLE_TO_MAP(E0063),
TABLE_TO_MAP(E0067),
TABLE_TO_MAP(E0069),
TABLE_TO_MAP(E0070),
TABLE_TO_MAP(E0071),
TABLE_TO_MAP(E0072),
TABLE_TO_MAP(E0073),
TABLE_TO_MAP(E0074),
TABLE_TO_MAP(E0075),
TABLE_TO_MAP(E0076),
TABLE_TO_MAP(E0077),
TABLE_TO_MAP(E0080),
TABLE_TO_MAP(E0081),
TABLE_TO_MAP(E0084),
TABLE_TO_MAP(E0087),
TABLE_TO_MAP(E0088),
TABLE_TO_MAP(E0089),
TABLE_TO_MAP(E0090),
TABLE_TO_MAP(E0091),
TABLE_TO_MAP(E0092),
TABLE_TO_MAP(E0093),
TABLE_TO_MAP(E0094),
TABLE_TO_MAP(E0106),
TABLE_TO_MAP(E0107),
TABLE_TO_MAP(E0109),
TABLE_TO_MAP(E0110),
TABLE_TO_MAP(E0116),
TABLE_TO_MAP(E0117),
TABLE_TO_MAP(E0118),
TABLE_TO_MAP(E0119),
TABLE_TO_MAP(E0120),
TABLE_TO_MAP(E0121),
TABLE_TO_MAP(E0124),
TABLE_TO_MAP(E0128),
TABLE_TO_MAP(E0130),
TABLE_TO_MAP(E0131),
TABLE_TO_MAP(E0132),
TABLE_TO_MAP(E0133),
TABLE_TO_MAP(E0136),
TABLE_TO_MAP(E0137),
TABLE_TO_MAP(E0138),
TABLE_TO_MAP(E0139),
TABLE_TO_MAP(E0152),
TABLE_TO_MAP(E0154),
TABLE_TO_MAP(E0158),
TABLE_TO_MAP(E0161),
TABLE_TO_MAP(E0162),
TABLE_TO_MAP(E0164),
TABLE_TO_MAP(E0165),
TABLE_TO_MAP(E0170),
TABLE_TO_MAP(E0178),
TABLE_TO_MAP(E0183),
TABLE_TO_MAP(E0184),
TABLE_TO_MAP(E0185),
TABLE_TO_MAP(E0186),
TABLE_TO_MAP(E0191),
TABLE_TO_MAP(E0192),
TABLE_TO_MAP(E0193),
TABLE_TO_MAP(E0195),
TABLE_TO_MAP(E0197),
TABLE_TO_MAP(E0198),
TABLE_TO_MAP(E0199),
TABLE_TO_MAP(E0200),
TABLE_TO_MAP(E0201),
TABLE_TO_MAP(E0203),
TABLE_TO_MAP(E0204),
TABLE_TO_MAP(E0205),
TABLE_TO_MAP(E0206),
TABLE_TO_MAP(E0207),
TABLE_TO_MAP(E0208),
TABLE_TO_MAP(E0210),
TABLE_TO_MAP(E0211),
TABLE_TO_MAP(E0212),
TABLE_TO_MAP(E0214),
TABLE_TO_MAP(E0220),
TABLE_TO_MAP(E0221),
TABLE_TO_MAP(E0222),
TABLE_TO_MAP(E0223),
TABLE_TO_MAP(E0224),
TABLE_TO_MAP(E0225),
TABLE_TO_MAP(E0226),
TABLE_TO_MAP(E0227),
TABLE_TO_MAP(E0228),
TABLE_TO_MAP(E0229),
TABLE_TO_MAP(E0230),
TABLE_TO_MAP(E0231),
TABLE_TO_MAP(E0232),
TABLE_TO_MAP(E0243),
TABLE_TO_MAP(E0244),
TABLE_TO_MAP(E0251),
TABLE_TO_MAP(E0252),
TABLE_TO_MAP(E0253),
TABLE_TO_MAP(E0254),
TABLE_TO_MAP(E0255),
TABLE_TO_MAP(E0256),
TABLE_TO_MAP(E0259),
TABLE_TO_MAP(E0260),
TABLE_TO_MAP(E0261),
TABLE_TO_MAP(E0262),
TABLE_TO_MAP(E0263),
TABLE_TO_MAP(E0264),
TABLE_TO_MAP(E0267),
TABLE_TO_MAP(E0268),
TABLE_TO_MAP(E0271),
TABLE_TO_MAP(E0275),
TABLE_TO_MAP(E0276),
TABLE_TO_MAP(E0277),
TABLE_TO_MAP(E0281),
TABLE_TO_MAP(E0282),
TABLE_TO_MAP(E0283),
TABLE_TO_MAP(E0284),
TABLE_TO_MAP(E0297),
TABLE_TO_MAP(E0301),
TABLE_TO_MAP(E0302),
TABLE_TO_MAP(E0303),
TABLE_TO_MAP(E0307),
TABLE_TO_MAP(E0308),
TABLE_TO_MAP(E0309),
TABLE_TO_MAP(E0310),
TABLE_TO_MAP(E0311),
TABLE_TO_MAP(E0312),
TABLE_TO_MAP(E0316),
TABLE_TO_MAP(E0317),
TABLE_TO_MAP(E0320),
TABLE_TO_MAP(E0321),
TABLE_TO_MAP(E0322),
TABLE_TO_MAP(E0323),
TABLE_TO_MAP(E0324),
TABLE_TO_MAP(E0325),
TABLE_TO_MAP(E0326),
TABLE_TO_MAP(E0328),
TABLE_TO_MAP(E0329),
TABLE_TO_MAP(E0364),
TABLE_TO_MAP(E0365),
TABLE_TO_MAP(E0366),
TABLE_TO_MAP(E0367),
TABLE_TO_MAP(E0368),
TABLE_TO_MAP(E0369),
TABLE_TO_MAP(E0370),
TABLE_TO_MAP(E0371),
TABLE_TO_MAP(E0373),
TABLE_TO_MAP(E0374),
TABLE_TO_MAP(E0375),
TABLE_TO_MAP(E0376),
TABLE_TO_MAP(E0377),
TABLE_TO_MAP(E0378),
TABLE_TO_MAP(E0379),
TABLE_TO_MAP(E0380),
TABLE_TO_MAP(E0381),
TABLE_TO_MAP(E0382),
TABLE_TO_MAP(E0383),
TABLE_TO_MAP(E0384),
TABLE_TO_MAP(E0386),
TABLE_TO_MAP(E0387),
TABLE_TO_MAP(E0388),
TABLE_TO_MAP(E0389),
TABLE_TO_MAP(E0390),
TABLE_TO_MAP(E0391),
TABLE_TO_MAP(E0392),
TABLE_TO_MAP(E0393),
TABLE_TO_MAP(E0398),
TABLE_TO_MAP(E0399),
TABLE_TO_MAP(E0401),
TABLE_TO_MAP(E0403),
TABLE_TO_MAP(E0404),
TABLE_TO_MAP(E0405),
TABLE_TO_MAP(E0407),
TABLE_TO_MAP(E0408),
TABLE_TO_MAP(E0409),
TABLE_TO_MAP(E0411),
TABLE_TO_MAP(E0412),
TABLE_TO_MAP(E0415),
TABLE_TO_MAP(E0416),
TABLE_TO_MAP(E0422),
TABLE_TO_MAP(E0423),
TABLE_TO_MAP(E0424),
TABLE_TO_MAP(E0425),
TABLE_TO_MAP(E0426),
TABLE_TO_MAP(E0428),
TABLE_TO_MAP(E0429),
TABLE_TO_MAP(E0430),
TABLE_TO_MAP(E0431),
TABLE_TO_MAP(E0432),
TABLE_TO_MAP(E0433),
TABLE_TO_MAP(E0434),
TABLE_TO_MAP(E0435),
TABLE_TO_MAP(E0436),
TABLE_TO_MAP(E0437),
TABLE_TO_MAP(E0438),
TABLE_TO_MAP(E0439),
TABLE_TO_MAP(E0445),
TABLE_TO_MAP(E0446),
TABLE_TO_MAP(E0447),
TABLE_TO_MAP(E0448),
TABLE_TO_MAP(E0449),
TABLE_TO_MAP(E0451),
TABLE_TO_MAP(E0452),
TABLE_TO_MAP(E0453),
TABLE_TO_MAP(E0454),
TABLE_TO_MAP(E0455),
TABLE_TO_MAP(E0457),
TABLE_TO_MAP(E0458),
TABLE_TO_MAP(E0459),
TABLE_TO_MAP(E0460),
TABLE_TO_MAP(E0461),
TABLE_TO_MAP(E0462),
TABLE_TO_MAP(E0463),
TABLE_TO_MAP(E0464),
TABLE_TO_MAP(E0466),
TABLE_TO_MAP(E0468),
TABLE_TO_MAP(E0469),
TABLE_TO_MAP(E0472),
TABLE_TO_MAP(E0476),
TABLE_TO_MAP(E0477),
TABLE_TO_MAP(E0478),
TABLE_TO_MAP(E0482),
TABLE_TO_MAP(E0491),
TABLE_TO_MAP(E0492),
TABLE_TO_MAP(E0493),
TABLE_TO_MAP(E0495),
TABLE_TO_MAP(E0496),
TABLE_TO_MAP(E0497),
TABLE_TO_MAP(E0498),
TABLE_TO_MAP(E0499),
TABLE_TO_MAP(E0500),
TABLE_TO_MAP(E0501),
TABLE_TO_MAP(E0502),
TABLE_TO_MAP(E0503),
TABLE_TO_MAP(E0504),
TABLE_TO_MAP(E0505),
TABLE_TO_MAP(E0506),
TABLE_TO_MAP(E0507),
TABLE_TO_MAP(E0508),
TABLE_TO_MAP(E0509),
TABLE_TO_MAP(E0510),
TABLE_TO_MAP(E0511),
TABLE_TO_MAP(E0512),
TABLE_TO_MAP(E0514),
TABLE_TO_MAP(E0515),
TABLE_TO_MAP(E0516),
TABLE_TO_MAP(E0517),
TABLE_TO_MAP(E0518),
TABLE_TO_MAP(E0519),
TABLE_TO_MAP(E0520),
TABLE_TO_MAP(E0521),
TABLE_TO_MAP(E0522),
TABLE_TO_MAP(E0523),
TABLE_TO_MAP(E0524),
TABLE_TO_MAP(E0525),
TABLE_TO_MAP(E0527),
TABLE_TO_MAP(E0528),
TABLE_TO_MAP(E0529),
TABLE_TO_MAP(E0530),
TABLE_TO_MAP(E0531),
TABLE_TO_MAP(E0532),
TABLE_TO_MAP(E0533),
TABLE_TO_MAP(E0534),
TABLE_TO_MAP(E0535),
TABLE_TO_MAP(E0536),
TABLE_TO_MAP(E0537),
TABLE_TO_MAP(E0538),
TABLE_TO_MAP(E0539),
TABLE_TO_MAP(E0541),
TABLE_TO_MAP(E0542),
TABLE_TO_MAP(E0543),
TABLE_TO_MAP(E0544),
TABLE_TO_MAP(E0545),
TABLE_TO_MAP(E0546),
TABLE_TO_MAP(E0547),
TABLE_TO_MAP(E0549),
TABLE_TO_MAP(E0550),
TABLE_TO_MAP(E0551),
TABLE_TO_MAP(E0552),
TABLE_TO_MAP(E0554),
TABLE_TO_MAP(E0556),
TABLE_TO_MAP(E0557),
TABLE_TO_MAP(E0559),
TABLE_TO_MAP(E0560),
TABLE_TO_MAP(E0561),
TABLE_TO_MAP(E0562),
TABLE_TO_MAP(E0565),
TABLE_TO_MAP(E0566),
TABLE_TO_MAP(E0567),
TABLE_TO_MAP(E0568),
TABLE_TO_MAP(E0569),
TABLE_TO_MAP(E0570),
TABLE_TO_MAP(E0571),
TABLE_TO_MAP(E0572),
TABLE_TO_MAP(E0573),
TABLE_TO_MAP(E0574),
TABLE_TO_MAP(E0575),
TABLE_TO_MAP(E0576),
TABLE_TO_MAP(E0577),
TABLE_TO_MAP(E0578),
TABLE_TO_MAP(E0579),
TABLE_TO_MAP(E0580),
TABLE_TO_MAP(E0581),
TABLE_TO_MAP(E0582),
TABLE_TO_MAP(E0583),
TABLE_TO_MAP(E0584),
TABLE_TO_MAP(E0585),
TABLE_TO_MAP(E0586),
TABLE_TO_MAP(E0587),
TABLE_TO_MAP(E0588),
TABLE_TO_MAP(E0589),
TABLE_TO_MAP(E0590),
TABLE_TO_MAP(E0591),
TABLE_TO_MAP(E0592),
TABLE_TO_MAP(E0593),
TABLE_TO_MAP(E0594),
TABLE_TO_MAP(E0595),
TABLE_TO_MAP(E0596),
TABLE_TO_MAP(E0597),
TABLE_TO_MAP(E0599),
TABLE_TO_MAP(E0600),
TABLE_TO_MAP(E0601),
TABLE_TO_MAP(E0602),
TABLE_TO_MAP(E0603),
TABLE_TO_MAP(E0604),
TABLE_TO_MAP(E0605),
TABLE_TO_MAP(E0606),
TABLE_TO_MAP(E0607),
TABLE_TO_MAP(E0608),
TABLE_TO_MAP(E0609),
TABLE_TO_MAP(E0610),
TABLE_TO_MAP(E0614),
TABLE_TO_MAP(E0615),
TABLE_TO_MAP(E0616),
TABLE_TO_MAP(E0617),
TABLE_TO_MAP(E0618),
TABLE_TO_MAP(E0619),
TABLE_TO_MAP(E0620),
TABLE_TO_MAP(E0621),
TABLE_TO_MAP(E0622),
TABLE_TO_MAP(E0623),
TABLE_TO_MAP(E0624),
TABLE_TO_MAP(E0625),
TABLE_TO_MAP(E0626),
TABLE_TO_MAP(E0627),
TABLE_TO_MAP(E0628),
TABLE_TO_MAP(E0631),
TABLE_TO_MAP(E0632),
TABLE_TO_MAP(E0633),
TABLE_TO_MAP(E0634),
TABLE_TO_MAP(E0635),
TABLE_TO_MAP(E0636),
TABLE_TO_MAP(E0637),
TABLE_TO_MAP(E0638),
TABLE_TO_MAP(E0639),
TABLE_TO_MAP(E0640),
TABLE_TO_MAP(E0641),
TABLE_TO_MAP(E0642),
TABLE_TO_MAP(E0643),
TABLE_TO_MAP(E0644),
TABLE_TO_MAP(E0646),
TABLE_TO_MAP(E0647),
TABLE_TO_MAP(E0648),
TABLE_TO_MAP(E0657),
TABLE_TO_MAP(E0658),
TABLE_TO_MAP(E0659),
TABLE_TO_MAP(E0660),
TABLE_TO_MAP(E0661),
TABLE_TO_MAP(E0662),
TABLE_TO_MAP(E0663),
TABLE_TO_MAP(E0664),
TABLE_TO_MAP(E0665),
TABLE_TO_MAP(E0666),
TABLE_TO_MAP(E0667),
TABLE_TO_MAP(E0668),
TABLE_TO_MAP(E0669),
TABLE_TO_MAP(E0670),
TABLE_TO_MAP(E0671),
TABLE_TO_MAP(E0687),
TABLE_TO_MAP(E0688),
TABLE_TO_MAP(E0689),
TABLE_TO_MAP(E0690),
TABLE_TO_MAP(E0691),
TABLE_TO_MAP(E0692),
TABLE_TO_MAP(E0693),
TABLE_TO_MAP(E0695),
TABLE_TO_MAP(E0696),
TABLE_TO_MAP(E0697),
TABLE_TO_MAP(E0698),
TABLE_TO_MAP(E0699),
TABLE_TO_MAP(E0700),
TABLE_TO_MAP(E0701),
TABLE_TO_MAP(E0703),
TABLE_TO_MAP(E0704),
TABLE_TO_MAP(E0705),
TABLE_TO_MAP(E0706),
TABLE_TO_MAP(E0708),
TABLE_TO_MAP(E0710),
TABLE_TO_MAP(E0711),
TABLE_TO_MAP(E0712),
TABLE_TO_MAP(E0713),
TABLE_TO_MAP(E0714),
TABLE_TO_MAP(E0715),
TABLE_TO_MAP(E0716),
TABLE_TO_MAP(E0717),
TABLE_TO_MAP(E0718),
TABLE_TO_MAP(E0719),
TABLE_TO_MAP(E0720),
TABLE_TO_MAP(E0722),
TABLE_TO_MAP(E0724),
TABLE_TO_MAP(E0725),
TABLE_TO_MAP(E0726),
TABLE_TO_MAP(E0727),
TABLE_TO_MAP(E0728),
TABLE_TO_MAP(E0729),
TABLE_TO_MAP(E0730),
TABLE_TO_MAP(E0731),
TABLE_TO_MAP(E0732),
TABLE_TO_MAP(E0733),
TABLE_TO_MAP(E0734),
TABLE_TO_MAP(E0735),
TABLE_TO_MAP(E0736),
TABLE_TO_MAP(E0737),
TABLE_TO_MAP(E0739),
TABLE_TO_MAP(E0740),
TABLE_TO_MAP(E0741),
TABLE_TO_MAP(E0742),
TABLE_TO_MAP(E0743),
TABLE_TO_MAP(E0744),
TABLE_TO_MAP(E0745),
TABLE_TO_MAP(E0746),
TABLE_TO_MAP(E0747),
TABLE_TO_MAP(E0748),
TABLE_TO_MAP(E0749),
TABLE_TO_MAP(E0750),
TABLE_TO_MAP(E0751),
TABLE_TO_MAP(E0752),
TABLE_TO_MAP(E0753),
TABLE_TO_MAP(E0754),
TABLE_TO_MAP(E0755),
TABLE_TO_MAP(E0756),
TABLE_TO_MAP(E0757),
TABLE_TO_MAP(E0758),
TABLE_TO_MAP(E0759),
TABLE_TO_MAP(E0760),
TABLE_TO_MAP(E0761),
TABLE_TO_MAP(E0762),
TABLE_TO_MAP(E0763),
TABLE_TO_MAP(E0764),
TABLE_TO_MAP(E0765),
TABLE_TO_MAP(E0766),
TABLE_TO_MAP(E0767),
TABLE_TO_MAP(E0768),
TABLE_TO_MAP(E0769),
TABLE_TO_MAP(E0770),
TABLE_TO_MAP(E0771),
TABLE_TO_MAP(E0772),
TABLE_TO_MAP(E0773),
TABLE_TO_MAP(E0774),
TABLE_TO_MAP(E0775),
TABLE_TO_MAP(E0776),
TABLE_TO_MAP(E0777),
TABLE_TO_MAP(E0778),
TABLE_TO_MAP(E0779),
TABLE_TO_MAP(E0780),
TABLE_TO_MAP(E0781),
TABLE_TO_MAP(E0782),
TABLE_TO_MAP(E0783),
TABLE_TO_MAP(E0784),
TABLE_TO_MAP(E0785),
TABLE_TO_MAP(E0786),
TABLE_TO_MAP(E0787),
TABLE_TO_MAP(E0788),
TABLE_TO_MAP(E0789),
TABLE_TO_MAP(E0790),
TABLE_TO_MAP(E0791),
TABLE_TO_MAP(E0792),
TABLE_TO_MAP(E0793),
TABLE_TO_MAP(E0794),
};

extern void
rust_internal_error_at (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_error_at (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void
rust_error_at (const location_t, const ErrorCode, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void
rust_warning_at (const location_t, int opt, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void
rust_fatal_error (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_inform (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);

// rich locations
extern void
rust_error_at (const rich_location &, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void
rust_error_at (const rich_location &, const ErrorCode, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
// clang-format on

// These interfaces provide a way for the front end to ask for
// the open/close quote characters it should use when formatting
// diagnostics (warnings, errors).
extern const char *
rust_open_quote ();
extern const char *
rust_close_quote ();

// These interfaces are used by utilities above to pass warnings and
// errors (once format specifiers have been expanded) to the back end,
// and to determine quoting style. Avoid calling these routines directly;
// instead use the equivalent routines above. The back end is required to
// implement these routines.

// clang-format off
extern void
rust_be_internal_error_at (const location_t, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_error_at (const location_t, const std::string &errmsg);
extern void
rust_be_error_at (const location_t, const ErrorCode,
		  const std::string &errmsg);
extern void
rust_be_error_at (const rich_location &, const std::string &errmsg);
extern void
rust_be_error_at (const rich_location &, const ErrorCode,
		  const std::string &errmsg);
extern void
rust_be_warning_at (const location_t, int opt, const std::string &warningmsg);
extern void
rust_be_fatal_error (const location_t, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_inform (const location_t, const std::string &infomsg);
extern void
rust_be_get_quotechars (const char **open_quote, const char **close_quote);
extern bool
rust_be_debug_p (void);
// clang-format on

namespace Rust {
/* A structure used to represent an error. Useful for enabling
 * errors to be ignored, e.g. if backtracking. */
struct Error
{
  enum class Kind
  {
    Hint,
    Err,
    FatalErr,
  };

  Kind kind;
  location_t locus;
  std::string message;
  // TODO: store more stuff? e.g. node id?

  Error (Kind kind, location_t locus, std::string message)
    : kind (kind), locus (locus), message (std::move (message))
  {
    message.shrink_to_fit ();
  }

  Error (location_t locus, std::string message)
  {
    Error (Kind::Err, locus, std::move (message));
  }

  static Error Hint (location_t locus, std::string message)
  {
    return Error (Kind::Hint, locus, std::move (message));
  }

  static Error Fatal (location_t locus, std::string message)
  {
    return Error (Kind::FatalErr, locus, std::move (message));
  }

  // TODO: the attribute part might be incorrect
  Error (location_t locus, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (2, 3)*/ RUST_ATTRIBUTE_GCC_DIAG (3, 4);

  /**
   * printf-like overload of Error::Hint
   */
  static Error Hint (location_t locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  /**
   * printf-like overload of Error::Fatal
   */
  static Error Fatal (location_t locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  void emit () const
  {
    switch (kind)
      {
      case Kind::Hint:
	rust_inform (locus, "%s", message.c_str ());
	break;
      case Kind::Err:
	rust_error_at (locus, "%s", message.c_str ());
	break;
      case Kind::FatalErr:
	rust_fatal_error (locus, "%s", message.c_str ());
	break;
      }
  }
};
} // namespace Rust

// rust_debug uses normal printf formatting, not GCC diagnostic formatting.
#define rust_debug(...) rust_debug_loc (UNDEF_LOCATION, __VA_ARGS__)

// rust_sorry_at wraps GCC diagnostic "sorry_at" to accept "Location" instead of
// "location_t"
#define rust_sorry_at(location, ...) sorry_at (location, __VA_ARGS__)

void
rust_debug_loc (const location_t location, const char *fmt,
		...) ATTRIBUTE_PRINTF_2;

#endif // !defined(RUST_DIAGNOSTICS_H)
