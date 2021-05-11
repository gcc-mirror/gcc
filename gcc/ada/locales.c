/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             L O C A L E S                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 2010-2021, Free Software Foundation, Inc.      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file provides OS-dependent support for the Ada.Locales package.    */

#include <locale.h>
#include <ctype.h>
#include <stddef.h>

typedef char char4 [4];

/* Table containing equivalences between ISO_639_1 codes and their ISO_639_3
   alpha-3 code plus their language name. */

static char* iso_639[] =
{
  "aa", "aar", "Afar",
  "ab", "abk", "Abkhazian",
  "ae", "ave", "Avestan",
  "af", "afr", "Afrikaans",
  "ak", "aka", "Akan",
  "am", "amh", "Amharic",
  "an", "arg", "Aragonese",
  "ar", "ara", "Arabic",
  "as", "asm", "Assamese",
  "av", "ava", "Avaric",
  "ay", "aym", "Aymara",
  "az", "aze", "Azerbaijani",

  "ba", "bak", "Bashkir",
  "be", "bel", "Belarusian",
  "bg", "bul", "Bulgarian",
  "bi", "bis", "Bislama",
  "bm", "bam", "Bambara",
  "bn", "ben", "Bengali",
  "bo", "bod", "Tibetan",
  "br", "bre", "Breton",
  "bs", "bos", "Bosnian",

  "ca", "cat", "Catalan",
  "ce", "che", "Chechen",
  "ch", "cha", "Chamorro",
  "co", "cos", "Corsican",
  "cr", "cre", "Cree",
  "cs", "ces", "Czech",
  "cu", "chu", "Church Slavic",
  "cv", "chv", "Chuvash",
  "cy", "cym", "Welsh",

  "da", "dan", "Danish",
  "de", "deu", "German",
  "dv", "div", "Divehi",
  "dz", "dzo", "Dzongkha",

  "ee", "ewe", "Ewe",
  "el", "ell", "Modern Greek",
  "en", "eng", "English",
  "eo", "epo", "Esperanto",
  "es", "spa", "Spanish",
  "et", "est", "Estonian",
  "eu", "eus", "Basque",

  "fa", "fas", "Persian",
  "ff", "ful", "Fulah",
  "fi", "fin", "Finnish",
  "fj", "fij", "Fijian",
  "fo", "fao", "Faroese",
  "fr", "fra", "French",
  "fy", "fry", "Western Frisian",

  "ga", "gle", "Irish",
  "gd", "gla", "Scottish Gaelic",
  "gl", "glg", "Galician",
  "gn", "grn", "Guarani",
  "gu", "guj", "Gujarati",
  "gv", "glv", "Manx",

  "ha", "hau", "Hausa",
  "he", "heb", "Hebrew",
  "hi", "hin", "Hindi",
  "ho", "hmo", "Hiri Motu",
  "hr", "hrv", "Croatian",
  "ht", "hat", "Haitian",
  "hu", "hun", "Hungarian",
  "hy", "hye", "Armenian",
  "hz", "her", "Herero",

  "ia", "ina", "Interlingua",
  "id", "ind", "Indonesian",
  "ie", "ile", "Interlingue",
  "ig", "ibo", "Igbo",
  "ii", "iii", "Sichuan Yi",
  "ik", "ipk", "Inupiaq",
  "io", "ido", "Ido",
  "is", "isl", "Icelandic",
  "it", "ita", "Italian",
  "iu", "iku", "Inuktitut",

  "ja", "jpn", "Japanese",
  "jv", "jav", "Javanese",

  "ka", "kat", "Georgian",
  "kg", "kon", "Kongo",
  "ki", "kik", "Kikuyu",
  "kj", "kua", "Kuanyama",
  "kk", "kaz", "Kazakh",
  "kl", "kal", "Kalaallisut",
  "km", "khm", "Central Khmer",
  "kn", "kan", "Kannada",
  "ko", "kor", "Korean",
  "kr", "kau", "Kanuri",
  "ks", "kas", "Kashmiri",
  "ku", "kur", "Kurdish",
  "kv", "kom", "Komi",
  "kw", "cor", "Cornish",
  "ky", "kir", "Kirghiz",

  "la", "lat", "Latin",
  "lb", "ltz", "Luxembourgish",
  "lg", "lug", "Ganda",
  "li", "lim", "Limburgan",
  "ln", "lin", "Lingala",
  "lo", "lao", "Lao",
  "lt", "lit", "Lithuanian",
  "lu", "lub", "Luba-Katanga",
  "lv", "lav", "Latvian",

  "mg", "mlg", "Malagasy",
  "mh", "mah", "Marshallese",
  "mi", "mri", "Maori",
  "mk", "mkd", "Macedonian",
  "ml", "mal", "Malayalam",
  "mn", "mon", "Mongolian",
  "mr", "mar", "Marathi",
  "ms", "msa", "Malay",
  "mt", "mlt", "Maltese",
  "my", "mya", "Burmese",

  "na", "nau", "Nauru",
  "nb", "nob", "Norwegian Bokmal",
  "nd", "nde", "North Ndebele",
  "ne", "nep", "Nepali",
  "ng", "ndo", "Ndonga",
  "nl", "nld", "Dutch",
  "nn", "nno", "Norwegian Nynorsk",
  "no", "nor", "Norwegian",
  "nr", "nbl", "South Ndebele",
  "nv", "nav", "Navajo",
  "ny", "nya", "Nyanja",

  "oc", "oci", "Occitan",
  "oj", "oji", "Ojibwa",
  "om", "orm", "Oromo",
  "or", "ori", "Oriya",
  "os", "oss", "Ossetian",

  "pa", "pan", "Panjabi",
  "pi", "pli", "Pali",
  "pl", "pol", "Polish",
  "ps", "pus", "Pushto",
  "pt", "por", "Portuguese",

  "qu", "que", "Quechua",

  "rm", "roh", "Romansh",
  "rn", "run", "Rundi",
  "ro", "ron", "Romanian",
  "ru", "rus", "Russian",
  "rw", "kin", "Kinyarwanda",

  "sa", "san", "Sanskrit",
  "sc", "srd", "Sardinian",
  "sd", "snd", "Sindhi",
  "se", "sme", "Northern Sami",
  "sg", "sag", "Sango",
  "sh", "hbs", "Serbo-Croatian",
  "si", "sin", "Sinhala",
  "sk", "slk", "Slovak",
  "sl", "slv", "Slovenian",
  "sm", "smo", "Samoan",
  "sn", "sna", "Shona",
  "so", "som", "Somali",
  "sq", "sqi", "Albanian",
  "sr", "srp", "Serbian",
  "ss", "ssw", "Swati",
  "st", "sot", "Southern Sotho",
  "su", "sun", "Sundanese",
  "sv", "swe", "Swedish",
  "sw", "swa", "Swahili",

  "ta", "tam", "Tamil",
  "te", "tel", "Telugu",
  "tg", "tgk", "Tajik",
  "th", "tha", "Thai",
  "ti", "tir", "Tigrinya",
  "tk", "tuk", "Turkmen",
  "tl", "tgl", "Tagalog",
  "tn", "tsn", "Tswana",
  "to", "ton", "Tonga",
  "tr", "tur", "Turkish",
  "ts", "tso", "Tsonga",
  "tt", "tat", "Tatar",
  "tw", "twi", "Twi",
  "ty", "tah", "Tahitian",

  "ug", "uig", "Uighur",
  "uk", "ukr", "Ukrainian",
  "ur", "urd", "Urdu",
  "uz", "uzb", "Uzbek",

  "ve", "ven", "Venda",
  "vi", "vie", "Vietnamese",
  "vo", "vol", "Volapuk",

  "wa", "wln", "Walloon",
  "wo", "wol", "Wolof",

  "xh", "xho", "Xhosa",

  "yi", "yid", "Yiddish",
  "yo", "yor", "Yoruba",

  "za", "zha", "Zhuang",
  "zh", "zho", "Chinese",
  "zu", "zul", "Zulu"
};

/* Table containing equivalences between ISO_3166 alpha-2 codes and country
   names. This table has several entries for codes that have several valid
   country names. */

static char* iso_3166[] =
{
  "AU", "Australia",
  "AD", "Andorra",
  "AE", "United Arab Emirates",
  "AF", "Afghanistan",
  "AG", "Antigua and Barbuda",
  "AI", "Anguilla",
  "AL", "Albania",
  "AM", "Armenia",
  "AN", "Netherlands Antilles",
  "AO", "Angola",
  "AQ", "Antarctica",
  "AR", "Argentina",
  "AS", "American Samoa",
  "AT", "Austria",
  "AU", "Australia",
  "AW", "Aruba",
  "AX", "Aland Islands",
  "AZ", "Azerbaijan",

  "BA", "Bosnia and Herzegovina",
  "BB", "Barbados",
  "BD", "Bangladesh",
  "BE", "Belgium",
  "BF", "Burkina Faso",
  "BG", "Bulgaria",
  "BH", "Bahrain",
  "BI", "Burundi",
  "BJ", "Benin",
  "BL", "Saint Barthélemy",
  "BM", "Bermuda",
  "BN", "Brunei Darussalam",
  "BO", "Bolivia, Plurinational State of",
  "BQ", "Bonaire, Sint Eustatius and Saba",
  "BR", "Brazil",
  "BS", "Bahamas",
  "BT", "Bhutan",
  "BV", "Bouvet Island",
  "BW", "Botswana",
  "BY", "Belarus",
  "BZ", "Belize",

  "CA", "Canada",
  "CC", "Cocos (Keeling) Islands",
  "CD", "Congo, Democratic Republic of the",
  "CF", "Central African Republic",
  "CG", "Congo",
  "CH", "Switzerland",
  "CI", "Côte d'Ivoire",
  "CK", "Cook Islands",
  "CL", "Chile",
  "CM", "Cameroon",
  "CN", "China",
  "CN", "People’s Republic of China",
  "CN", "PR China",
  "CN", "PR-China",
  "CO", "Colombia",
  "CR", "Costa Rica",
  "CS", "Czechoslovakia",
  "CU", "Cuba",
  "CV", "Cape Verde",
  "CW", "Curaçao",
  "CX", "Christmas Island",
  "CY", "Cyprus",
  "CZ", "Czech Republic",

  "DE", "Germany",
  "DJ", "Djibouti",
  "DK", "Denmark",
  "DM", "Dominica",
  "DO", "Dominican Republic",
  "DZ", "Algeria",

  "EC", "Ecuador",
  "EE", "Estonia",
  "EG", "Egypt",
  "EH", "Western Sahara",
  "ER", "Eritrea",
  "ES", "Spain",
  "ET", "Ethiopia",

  "FI", "Finland",
  "FG", "Fiji",
  "FK", "Falkland Islands (Malvinas)",
  "FM", "Micronesia, Federated States of",
  "FO", "Faroe Islands",
  "FR", "France",

  "GA", "Gabon",
  "GB", "United Kingdom",
  "GB", "United-Kingdom",
  "GB", "England",
  "GB", "Britain",
  "GB", "Great Britain",
  "GD", "Grenada",
  "GE", "Georgia",
  "GF", "French Guiana",
  "GG", "Guernsey",
  "GH", "Ghana",
  "GI", "Gibraltar",
  "GL", "Greenland",
  "GM", "Gambia",
  "GN", "Guinea",
  "GP", "Guadeloupe",
  "GQ", "Equatorial Guinea",
  "GR", "Greece",
  "GS", "South Georgia and the South Sandwich Islands",
  "GT", "Guatemala",
  "GU", "Guam",
  "GW", "Guinea-Bissau",
  "GY", "Guyana",

  "HK", "Hong Kong",
  "HK", "Hong-Kong",
  "HM", "Heard Island and McDonald Islands",
  "HN", "Honduras",
  "HR", "Croatia",
  "HT", "Haiti",
  "HU", "Hungary",

  "ID", "Indonesia",
  "IE", "Ireland",
  "IL", "Israel",
  "IM", "Isle of Man",
  "IN", "India",
  "IO", "British Indian Ocean Territory",
  "IQ", "Iraq",
  "IR", "Iran",
  "IR", "Iran, Islamic Republic of",
  "IS", "Iceland",
  "IT", "Italy",

  "JE", "Jersey",
  "JM", "Jamaica",
  "JO", "Jordan",
  "JP", "Japan",

  "KE", "Kenya",
  "KG", "Kyrgyzstan",
  "KH", "Cambodia",
  "KI", "Kiribati",
  "KM", "Comoros",
  "KN", "Saint Kitts and Nevis",
  "KP", "Korea, Democratic People's Republic of",
  "KR", "Korea, Republic of",
  "KW", "Kuwait",
  "KY", "Cayman Islands",
  "KZ", "Kazakhstan",

  "LA", "Lao People's Democratic Republic",
  "LB", "Lebanon",
  "LC", "Saint Lucia",
  "LI", "Liechtenstein",
  "LK", "Sri Lanka",
  "LR", "Liberia",
  "LS", "Lesotho",
  "LT", "Lithuania",
  "LU", "Luxembourg",
  "LV", "Latvia",
  "LY", "Libya",

  "MA", "Morocco",
  "MC", "Monaco",
  "MD", "Moldova, Republic of",
  "ME", "Montenegro",
  "MF", "Saint Martin",
  "MG", "Madagascar",
  "MH", "Marshall Islands",
  "MK", "Macedonia",
  "ML", "Mali",
  "MM", "Myanmar",
  "MN", "Mongolia",
  "MO", "Macao",
  "MP", "Northern Mariana Islands",
  "MQ", "Martinique",
  "MR", "Mauritania",
  "MS", "Montserrat",
  "MT", "Malta",
  "MU", "Mauritius",
  "MV", "Maldives",
  "MW", "Malawi",
  "MX", "Mexico",
  "MY", "Malaysia",
  "MZ", "Mozambique",

  "NA", "Namibia",
  "NC", "New Caledonia",
  "NE", "Niger",
  "NF", "Norfolk Island",
  "NG", "Nigeria",
  "NI", "Nicaragua",
  "NL", "Netherlands",
  "NL", "Holland",
  "NO", "Norway",
  "NP", "Nepal",
  "NR", "Nauru",
  "NU", "Niue",
  "NZ", "New Zealand",
  "NZ", "New-Zealand",

  "OM", "Oman",

  "PA", "Panama",
  "PE", "Peru",
  "PF", "French Polynesia",
  "PG", "Papua New Guinea",
  "PH", "Philippines",
  "PK", "Pakistan",
  "PL", "Poland",
  "PM", "Saint Pierre and Miquelon",
  "PN", "Pitcairn",
  "PR", "Puerto Rico",
  "PS", "Palestine, State of",
  "PT", "Portugal",
  "PW", "Palau",
  "PY", "Paraguay",

  "QA", "Qatar",

  "RE", "Réunion",
  "RO", "Romania",
  "RS", "Serbia",
  "RU", "Russian Federation",
  "RW", "Rwanda",

  "SA", "Saudi Arabia",
  "SB", "Solomon Islands",
  "SC", "Seychelles",
  "SD", "Sudan",
  "SE", "Sweden",
  "SG", "Singapore",
  "SH", "Saint Helena, Ascension and Tristan da Cunha",
  "SI", "Slovenia",
  "SJ", "Svalbard and Jan Mayen",
  "SK", "Slovakia",
  "SL", "Sierra Leone",
  "SM", "San Marino",
  "SN", "Senegal",
  "SO", "Somalia",
  "SR", "Suriname",
  "SS", "South Sudan",
  "SV", "El Salvador",
  "SX", "Sint Maarten (Dutch part)",
  "SY", "Syrian Arab Republic",
  "SZ", "Swaziland",

  "TC", "Turks and Caicos Islands",
  "TD", "Chad",
  "TF", "French Southern Territories",
  "TG", "Togo",
  "TH", "Thailand",
  "TJ", "Tajikistan",
  "TK", "Tokelau",
  "TL", "Timor-Leste",
  "TM", "Turkmenistan",
  "TN", "Tunisia",
  "TO", "Tonga",
  "TP", "East Timor",
  "TR", "Turkey",
  "TT", "Trinidad and Tobago",
  "TV", "Tuvalu",
  "TW", "Taiwan",
  "TW", "Taiwan, Province of China",
  "TZ", "Tanzania",
  "TZ", "Tanzania, United Republic of",

  "UA", "Ukraine",
  "UG", "Uganda",
  "UM", "United States Minor Outlying Islands",
  "US", "United States",
  "US", "United States of America",
  "US", "United-States",
  "UY", "Uruguay",
  "UZ", "Uzbekistan",

  "VA", "Holy See (Vatican City State)",
  "VC", "Saint Vincent and the Grenadines",
  "VE", "Venezuela",
  "VE", "Venezuela, Bolivarian Republic of",
  "VG", "Virgin Islands, British",
  "VI", "Virgin Islands, U.S.",
  "VN", "Viet Nam",
  "VU", "Vanuatu",
  "WF", "Wallis and Futuna",
  "WS", "Samoa",

  "YE", "Yemen",
  "YT", "Mayotte",
  "YU", "Yugoslavia",

  "ZA", "South Africa",
  "ZM", "Zambia",
  "ZW", "Zimbabwe"
};

/* Utility function to perform case insensitive string comparison. Returns 1
   if both strings are equal and 0 otherwise. */

static int
str_case_equals (const char *s1, const char *s2) {
  while (*s1 != '\0' && *s2 != '\0' && tolower(*s1) == tolower(*s2)) {
    s1++;
    s2++;
  }

  return (*s1 == '\0') && (*s2 == '\0');
}

/* Utility function to copy length characters of a string. The target string
   must have space to store the extra string null terminator. */

static void
str_copy (char *target, char *source, int length) {
  for (; length > 0; source++, target++, length--) {
    *target = *source;
  }

  *target = '\0';
}

/* Utility function to search for the last byte of the lc_all string to be
   processed. Required because in some targets (for example, AIX), the
   string returned by setlocale() has duplicates. */

static char*
str_get_last_byte (char *lc_all) {
  char* first_space = NULL;
  char* second_space = NULL;
  char* last_byte = NULL;
  char* s1 = lc_all;

  /* Search for the 1st space (if any) */
  while (*s1 != ' ' && *s1 != '\0')
    s1++;

  if (*s1 == '\0') {
    last_byte = s1;

  } else {
    first_space = s1;

    /* Skip this space and search for the 2nd one (if available) */
    s1++;
    while (*s1 != ' ' && *s1 != '\0')
      s1++;

    if (*s1 == '\0') {
      last_byte = s1;

    } else {
      second_space=s1;

      /* Search for the last byte of lc_all */
      while (*s1 != '\0')
        s1++;

      last_byte = s1;

      /* Check if the two strings match */
      {
        int len1 = first_space - lc_all;
        int len2 = second_space - first_space - 1;

        if (len1 == len2) {
          char* p1 = lc_all;
          char* p2 = first_space + 1;

          /* Compare their contents */
          while (*p1 == *p2 && p2 != second_space) {
            p1++;
            p2++;
          }

          /* if the two strings match then update the last byte */

          if (p2 == second_space) {
            last_byte = first_space;
          }
        }
      }
    }
  }

  return last_byte;
}

/* Utility function to search in the iso_639_1 table for an iso-639-1 code;
   returns the corresponding iso-639-3 code or NULL if not found. */

static char*
iso_639_1_to_639_3(char* iso_639_1_code) {
  int len = sizeof(iso_639)/sizeof(iso_639[0]);
  char **p = iso_639;
  int j;

  for (j=0; j < len/3; j++) {
    char* s1 = iso_639_1_code;
    char* s2 = *p;

    if (s1[0]==s2[0] && s1[1]==s2[1]) {
      p++;
      return *p;
    }

    p = p + 3;
  }

  return NULL;
}

/* Utility function to search in the iso_639_1 table for a language name;
   returns the corresponding iso-639-3 code or NULL if not found. */

static char*
language_name_to_639_3(char* name) {
  int len = sizeof(iso_639)/sizeof(iso_639[0]);
  char **p = iso_639;
  int j;

  p = p + 2;
  for (j=0; j < len/3; j++) {
    if (str_case_equals(name, *p)) {
      p--;
      return *p;
    }

    p = p + 3;
  }

  return NULL;
}

/* Utility function to search in the iso_3166 table for a country name;
   returns the corresponding iso-3166 code or NULL if not found. */

static char*
country_name_to_3166 (char* name) {
  int len = sizeof(iso_3166)/sizeof(iso_3166[0]);
  char **p = iso_3166;
  int j;

  p++;
  for (j=0; j < len/2; j++) {
    if (str_case_equals(name, *p)) {
      p--;
      return *p;
    }

    p = p + 2;
  }

  return NULL;
}

/*
  c_get_language_code needs to fill in the Alpha-3 encoding of the
  language code (3 lowercase letters). That should be "und" if the
  language is unknown. [see Ada.Locales]
*/
void
c_get_language_code (char4 p) {
  char* Saved_Locale = setlocale(LC_ALL, NULL);
  char  iso_639_3_code[] = "und";                        /* Language Unknown */
  char* lc_all;
  char* result;

  /* Get locales set in the environment */

  setlocale(LC_ALL, "");
  lc_all = setlocale(LC_ALL, NULL);

  /* The string returned by setlocale has the following format:

           language[_territory][.code-set][@modifier]

     where language is an ISO 639 language code, territory is an ISO 3166
     country code, and codeset is a character set or encoding identifier
     like ISO-8859-1 or UTF-8.
  */

  if (lc_all != NULL) {
    char* s = lc_all;
    int lang_length = 0;

    /* Copy the language part (which may be an ISO-639-1 code, an ISO-639-3
       code, or a language name) adding a string terminator */

    while (*s != '_' && *s != '.' && *s != '@' && *s != '\0')
      s++;

    lang_length = s - lc_all;

    /* Handle conversion of ISO-639-1 to ISO-639-3 */

    if (lang_length == 2) {
      char  iso_639_1[3];
      char* to_iso_639_3;

      /* Duplicate the ISO-639-1 code adding the null terminator required to
         search for the equivalent ISO-639-3 code; we cannot just append the
         null terminator since the pointer may reference non-writable memory.
      */

      str_copy(iso_639_1, lc_all, lang_length);
      to_iso_639_3 = iso_639_1_to_639_3(iso_639_1);

      if (to_iso_639_3)
        str_copy(iso_639_3_code, to_iso_639_3, 3);

    /* Copy the ISO-639-3 code (adding a null terminator) */

    } else if (lang_length == 3) {
      str_copy(iso_639_3_code, lc_all, lang_length);

    /* Handle conversion of language name to ISO-639-3 */

    } else if (lang_length > 3) {
      char  name_copy[lang_length + 1];
      char* to_iso_639_3;

      /* Duplicate the ISO-639-1 code adding the null terminator required to
         search for the equivalent ISO-639-3 code; we cannot just append the
         null terminator since the pointer may reference non-writable memory.
      */

      str_copy(name_copy, lc_all, lang_length);
      to_iso_639_3 = language_name_to_639_3(name_copy);

      if (to_iso_639_3)
        str_copy(iso_639_3_code, to_iso_639_3, 3);
    }
  }

  /* Copy out the computed ISO_639_3 code */

  result = iso_639_3_code;
  for (; *result != '\0'; p++, result++)
    *p = *result;

  /* Restore the original locale settings */

  setlocale(LC_ALL, Saved_Locale);

  return;
}

/*
  c_get_country_code needs to fill in the Alpha-2 encoding of the
  country code (2 uppercase letters). That should be "ZZ" if the
  country is unknown. [see Ada.Locales]
*/
void
c_get_country_code (char4 p) {
  char* Saved_Locale = setlocale(LC_ALL, NULL);
  char  iso_3166_code[] = "ZZ";                           /* Country Unknown */
  char* lc_all;
  char* result;

  /* Get locales set in the environment */

  setlocale(LC_ALL, "");
  lc_all = setlocale(LC_ALL, NULL);

  /* The string returned by setlocale has the following format:

           language[_territory][.code-set][@modifier]

     where language is an ISO 639 language code, territory is an ISO 3166
     country code, and codeset is a character set or encoding identifier
     like ISO-8859-1 or UTF-8.
  */

  if (lc_all != NULL) {
    char* s1 = lc_all;
    char* s2 = NULL;
    char* last_byte = str_get_last_byte(lc_all);
    int country_length = 0;

    /* Search for the beginning of the country code */

    s1 = lc_all;
    while (*s1 != '_' && *s1 != '.' && *s1 != '@' && s1 != last_byte)
      s1++;

    if (*s1 == '_') {
      s1++;
      s2 = s1;

      while (*s2 != '.' && *s2 != '@' && s2 != last_byte)
        s2++;

      country_length = s2 - s1;

      if (country_length == 2) {
        str_copy(iso_3166_code, s1, country_length);

      /* setlocale returned us the country name */

      } else if (country_length > 3) {
        char  name_copy[country_length + 1];
        char* to_3166;

        str_copy(name_copy, s1, country_length);
        to_3166 = country_name_to_3166(name_copy);

        if (to_3166)
          str_copy(iso_3166_code, to_3166, 2);
      }
    }
  }

  /* Copy out the computed ISO_3166 code */

  result = iso_3166_code;
  for (; *result != '\0'; p++, result++)
    *p = *result;

  /* Restore the original locale settings */

  setlocale(LC_ALL, Saved_Locale);

  return;
}
